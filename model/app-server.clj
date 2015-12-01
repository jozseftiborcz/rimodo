;; Sample model with a three tier application and servers
(require '[rimodo.model.server :refer :all])

(server app-server 
        :dns-name "app-server"
        :name "appserver01"
        :role "appserver"
        :ip "1.2.3.4")

(server "coredbsrv01" :role "dbserver")

(application
  netbank "Internet banking system")

(application netbank2 "Internet banking system")
(application netbank3 "Internet banking system")

(netbank (runs-on :db-tier "coredbsrv01" :apps-tier app-server))
(netbank2 (runs-on "coredbsrv01" app-server))
(netbank3 :runs-on "coredbsrv01" app-server)


