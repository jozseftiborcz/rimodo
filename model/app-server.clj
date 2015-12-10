;; Sample model with a three tier application and servers
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

(server appsrv1)
(server appsrv2)

(cluster-group "group1"
  :cluster-ip "1.1.1.1" appsrv1 appsrv2)

; it searches and returns an application
; (m-search :application "netbank2")
; this returns every application
; (m-search :application)

; it yields true if application
;(application? netbank2)
; create a linkage between two model element
; (m-link netbank app-server :apps-tier)

; this returns a seq of tuples (model-element, linkage-kind) of linkages
; (m-links netbank)
;
; the result of the function call is its actual result set
; (textualize (m-search :application "netbank2))
