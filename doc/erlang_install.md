
# on Debian 12

```

apt-get update

apt-get install -y --no-install-recommends curl git vim make gcc g++ apt-transport-https ca-certificates libodbc1 libssl3 libsctp1 autoconf dpkg-dev libncurses-dev unixodbc-dev libssl-dev libsctp-dev

wget https://github.com/erlang/otp/releases/download/OTP-26.2.5.3/otp_src_26.2.5.3.tar.gz

echo "c2707ce08e91235145cdfc487352f05570a2a0bddf1c478154549eb9e68805b0  otp_src_26.2.5.3.tar.gz" | sha256sum -c -


tar -xzf otp_src_26.2.5.3.tar.gz && cd otp_src_26.2.5.3
./otp_build autoconf
gnuArch="$(dpkg-architecture --query DEB_HOST_GNU_TYPE)" && ./configure --build="$gnuArch"

make && make install
````

# on CentOS8
```
sudo yum install -y gcc gcc-c++ glibc-devel make ncurses-devel openssl-devel autoconf java-1.8.0-openjdk-devel git perl wget
sudo yum -y install ncurses-devel mesa-libGLU-devel  openssl-devel  gtk3-devel

wget https://github.com/erlang/otp/archive/refs/tags/OTP-25.3.2.8.tar.gz

tar -zxf OTP-25.3.2.8.tar.gz
 cd otp-OTP-25.3.2.8/
 ./otp_build autoconf
./configure --prefix=/usr/local/erlang
make && make install

rm -rf /usr/local/bin/erl && rm -rf /usr/local/bin/erlc && rm -rf /usr/local/bin/epmd && rm -rf /usr/local/bin/run_erl && rm -rf /usr/local/bin/to_erl && rm -rf /usr/local/bin/dialyzer && rm -rf /usr/local/bin/typer && rm -rf /usr/local/bin/escript && rm -rf /usr/local/bin/ct_run

ln -s /usr/local/erlang/bin/erl /usr/local/bin/erl && ln -s /usr/local/erlang/bin/erlc /usr/local/bin/erlc && ln -s /usr/local/erlang/bin/epmd /usr/local/bin/epmd && ln -s /usr/local/erlang/bin/run_erl /usr/local/bin/run_erl && ln -s /usr/local/erlang/bin/to_erl /usr/local/bin/to_erl && ln -s /usr/local/erlang/bin/dialyzer /usr/local/bin/dialyzer && ln -s /usr/local/erlang/bin/typer /usr/local/bin/typer && ln -s /usr/local/erlang/bin/escript /usr/local/bin/escript && ln -s /usr/local/erlang/bin/ct_run /usr/local/bin/ct_run
```
