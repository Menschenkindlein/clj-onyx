(set-env!
 :repositories [["central" {:url "http://repo1.maven.org/maven2"}]
                ["clojars" "https://clojars.org/repo/"]
                ["clojars-mirror" "https://clojars-mirror.tcrawley.org/repo/"]]
 :dependencies '[[cheshire "5.5.0"]
                 [seesaw "1.4.5"]]
 :resource-paths #{"src"})

(require 'gui 'game 'ai)

(deftask play []
  (let [f (gui/make-game-frame)
        end (promise)
        listener (seesaw.core/listen
                  f :window-closing (fn [_] (deliver end true)))]
    (future
      (game/play-game (gui/make-gui-player
                       "Unstoppable genius"
                       gui/click-reader
                       f)
                      ai/minimax
                      #_
                      (gui/make-gui-player
                       "Wonderful thinker"
                       gui/click-reader
                       f)))
    (deref end)))
