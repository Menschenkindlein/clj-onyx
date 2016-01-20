(set-env!
 :repositories [["central" {:url "http://repo1.maven.org/maven2"}]
                ["clojars" "https://clojars.org/repo/"]
                ["clojars-mirror" "https://clojars-mirror.tcrawley.org/repo/"]]
 :dependencies '[[cheshire "5.5.0"]
                 [seesaw "1.4.5"]]
 :resource-paths #{"src"})
