
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
