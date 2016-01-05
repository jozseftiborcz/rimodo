;; Sample model with a three tier application and servers
(server app-server 
        :dns-name "app-server"
        :name "appserver01"
        :role "appserver"
        :ip "1.2.3.4")

(server "coredbsrv01" :role "dbserver")

