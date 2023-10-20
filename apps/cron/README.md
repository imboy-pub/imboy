

```

ecron:statistic().

ecron:add(test_job, "*/15 * * * * *", {demo_cron_logic, inspect, ["xxx"]}).


apply(demo_cron_logic, F, A).
ecron:activate(test_job).

ecron:statistic(test_job).
ecron:deactivate(test_job).

ecron:delete(test_job).

```

