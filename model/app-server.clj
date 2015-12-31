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
(if (not= (count (search :application "netbank2")) 1) (println "Error: netbank2 not found"))
;; regular expression works as well
(if (not= (count (search :server #"apps.*")) 2) (println "Error: appsrv1,2 not found"))
; this returns every application
(if (not= (count (search :application)) 3) (println "Error: not every application is found"))

; this creates a file app-server_name.te with the following content
; (textualize "name" (m-search :application #"2")
;   {:source "app-server.clj" :line 36}
;   :result
;   (application/netbank2))
(textualize "name" (search :application ))

; it yields true if application
;(application? netbank2)
; create a linkage between two model element
; (m-link netbank app-server :apps-tier)

; this returns a seq of tuples (model-element, linkage-kind) of linkages
; (m-links netbank)
;
; the result of the function call is its actual result set
; (textualize (m-search :application "netbank2))
; (donor netbank :what)
; (donate netbank2 :what)
; (donor netbank :to netbank2 :what)
; (inject-fn netbank what)
