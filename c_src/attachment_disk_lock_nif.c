#include "erl_nif.h"
#include "erl_driver.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

typedef struct { int fd; ErlNifMutex *mutex; char *target; } disk_lock;
static ErlNifResourceType *lock_type;

static ERL_NIF_TERM error(ErlNifEnv *env, int err) {
    return enif_make_tuple2(env, enif_make_atom(env, "error"),
                            enif_make_atom(env, erl_errno_id(err)));
}

static char *path(ErlNifEnv *env, ERL_NIF_TERM term) {
    ErlNifBinary b;
    if (!enif_inspect_binary(env, term, &b) || b.size == 0 ||
        memchr(b.data, 0, b.size)) return NULL;
    char *p = enif_alloc(b.size + 1);
    if (!p) return NULL;
    memcpy(p, b.data, b.size); p[b.size] = 0;
    return p;
}

static void destroy(ErlNifEnv *env, void *obj) {
    (void)env;
    disk_lock *l = obj;
    if (l->fd >= 0) close(l->fd);
    if (l->mutex) enif_mutex_destroy(l->mutex);
    if (l->target) enif_free(l->target);
}

static ERL_NIF_TERM acquire(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    (void)argc;
    char *p = path(env, argv[0]);
    char *target = path(env, argv[1]);
    if (!p || !target || target[0] != '/') {
        if (p) enif_free(p);
        if (target) enif_free(target);
        return enif_make_badarg(env);
    }
    int fd = open(p, O_RDWR | O_CREAT | O_NOFOLLOW | O_CLOEXEC, 0600);
    if (fd < 0) { int e = errno; enif_free(p); enif_free(target); return error(env, e); }
    struct stat held, named;
    if (fstat(fd, &held) || lstat(p, &named) || !S_ISREG(held.st_mode) ||
        held.st_nlink != 1 || held.st_uid != geteuid() || (held.st_mode & 0022) ||
        held.st_dev != named.st_dev || held.st_ino != named.st_ino) {
        close(fd); enif_free(p); enif_free(target); return error(env, EACCES);
    }
    if (flock(fd, LOCK_EX | LOCK_NB)) { int e = errno; close(fd); enif_free(p); enif_free(target); return error(env, e); }
    /* Recheck after acquiring: the name can be replaced while waiting. */
    if (fstat(fd, &held) || lstat(p, &named) || !S_ISREG(named.st_mode) ||
        held.st_nlink != 1 || held.st_uid != geteuid() || (held.st_mode & 0022) ||
        held.st_dev != named.st_dev || held.st_ino != named.st_ino) {
        close(fd); enif_free(p); enif_free(target); return error(env, EACCES);
    }
    enif_free(p);
    disk_lock *l = enif_alloc_resource(lock_type, sizeof(*l));
    if (!l) { close(fd); enif_free(target); return error(env, ENOMEM); }
    l->fd = fd; l->target = target; l->mutex = enif_mutex_create("attachment_disk_lock");
    if (!l->mutex) { close(fd); l->fd = -1; enif_release_resource(l); return error(env, ENOMEM); }
    ERL_NIF_TERM result = enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_resource(env, l));
    enif_release_resource(l);
    return result;
}

static ERL_NIF_TERM release(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    (void)argc;
    disk_lock *l;
    if (!enif_get_resource(env, argv[0], lock_type, (void **)&l)) return enif_make_badarg(env);
    enif_mutex_lock(l->mutex);
    if (l->fd >= 0) { close(l->fd); l->fd = -1; }
    enif_mutex_unlock(l->mutex);
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM mutate(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    disk_lock *l;
    if (!enif_get_resource(env, argv[0], lock_type, (void **)&l)) return enif_make_badarg(env);
    char *a = path(env, argv[1]);
    char *b = argc == 3 ? path(env, argv[2]) : NULL;
    if (!a || (argc == 3 && !b)) { if (a) enif_free(a); if (b) enif_free(b); return enif_make_badarg(env); }
    enif_mutex_lock(l->mutex);
    const char *dest = argc == 3 ? b : a;
    int valid = strcmp(dest, l->target) == 0;
    if (argc == 3) {
        size_t n = strlen(l->target);
        valid = valid && strncmp(a, l->target, n) == 0 &&
                strncmp(a + n, ".v5tmp.", 7) == 0 &&
                a[n + 7] != '\0' && strchr(a + n + 7, '/') == NULL;
    }
    int rc = l->fd < 0 || !valid ? -1 : (argc == 3 ? rename(a, b) : unlink(a));
    int e = l->fd < 0 ? EBADF : (!valid ? EACCES : errno);
    enif_mutex_unlock(l->mutex);
    enif_free(a); if (b) enif_free(b);
    return rc == 0 ? enif_make_atom(env, "ok") : error(env, e);
}

static int load(ErlNifEnv *env, void **data, ERL_NIF_TERM info) {
    (void)data; (void)info;
    lock_type = enif_open_resource_type(env, NULL, "attachment_disk_lock", destroy,
                                        ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);
    return lock_type ? 0 : 1;
}
static ErlNifFunc funcs[] = {
    {"acquire", 2, acquire, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"release", 1, release, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"rename", 3, mutate, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"unlink", 2, mutate, ERL_NIF_DIRTY_JOB_IO_BOUND}
};
ERL_NIF_INIT(attachment_disk_lock_nif, funcs, load, NULL, NULL, NULL)
