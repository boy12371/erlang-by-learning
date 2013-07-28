d:
cd D:\tcp_server
erl -sasl dir 'D:\tcp_server\lib\sasl-2.3.2\ebin' -eval "application:start(sasl)"  -name foo@127.0.0.1 -setcookies 1234 -s init stop
erl -pa D:\tcp_server\lib\tcp_server-1.0\ebin -boot D:\tcp_server\releases\1.0\start -eval "appmon:start()" -name foo@127.0.0.1 -setcookies 1234