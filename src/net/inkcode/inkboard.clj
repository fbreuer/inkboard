;;    InkBoard - a pen-centric on-screen keyboard
;;    Copyright (C) 2010-2011 Felix Breuer
;;
;;    This program is free software: you can redistribute it and/or modify
;;    it under the terms of the GNU General Public License as published by
;;    the Free Software Foundation, either version 3 of the License, or
;;    (at your option) any later version.
;;
;;    This program is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;    GNU General Public License for more details.
;;
;;    You should have received a copy of the GNU General Public License
;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.

(ns net.inkcode.inkboard (:gen-class))

(import '(java.awt Frame BasicStroke Font Graphics Graphics2D Toolkit Point)
  '(java.awt Robot Color Dimension RenderingHints)
  '(java.awt.event WindowListener WindowAdapter KeyEvent ActionEvent ActionListener)
  '(java.awt.geom Ellipse2D Ellipse2D$Double)
  '(java.awt.image BufferedImage BufferedImageOp MemoryImageSource)
  '(javax.imageio ImageIO)
  '(java.io File)
  '(javax.swing JFrame JPanel Timer KeyStroke ImageIcon BorderFactory)
  '(jpen.event PenListener)
  '(jpen.demo StatusReport)
  '(jpen PButtonEvent PenManager PKindEvent PLevelEvent PScrollEvent PLevel PLevel$Type))
(require 'clojure.walk)
(require '[clojure.contrib.profile :as prof])
(require '[clojure.contrib.string :as string])
(require '[clojure.contrib.generic.math-functions :as math])
(require '[clojure.contrib.import-static :as import-static])
(import-static/import-static java.lang.Math PI)

(set! *warn-on-reflection* true)

(defn assoc-in-mult
  ([s] s)
  ([s k v]
    (if (vector? k) 
      (assoc-in s k v)
      (assoc s k v)))
  ([s k v & rest] 
    (apply assoc-in-mult (cons (assoc-in-mult s k v) rest)))) 

(defmacro defstatetrafo [name state params & body]
  (let [old-state (gensym)
        tmp-state (gensym)
        state-updater (gensym)
        body-rewriter (fn [tree] 
			(clojure.walk/postwalk 
			 (fn [a] 
			   (cond 
			    (= a :=) state-updater
			    (and (list? a) (= (first a) (symbol "§"))) (concat `(-> ~old-state) (rest a))
			    :else a))
			 tree))]
    `(defn ~name ~params
       (let [~old-state (deref ~state)
	     ~tmp-state (ref ~old-state)
	     ~state-updater (fn [& args#]
			      (ref-set ~tmp-state (apply assoc-in-mult (deref ~tmp-state) args#)))]
	 (dosync ~@(map body-rewriter body))
         (dosync (ref-set ~state (deref ~tmp-state)))))))

(defrecord MenuLeaf [label action])

;; Nasty Virtual Keyboard hack

(def ^Robot robot (new Robot))

(defmacro hit-key [key] `(fn [] (. robot (keyPress (. KeyEvent ~key)))
                                (. robot (keyRelease (. KeyEvent ~key)))))

(defmacro hit-key-shift [key] `(fn []
				 (. robot (keyPress (. KeyEvent VK_SHIFT)))
				 (. robot (keyPress (. KeyEvent ~key)))
				 (. robot (keyRelease (. KeyEvent ~key)))
				 (. robot (keyRelease (. KeyEvent VK_SHIFT)))))

(defn type-numpad [digit] 
  ;(.. System out (println digit))
  (case digit
    0 ((hit-key VK_NUMPAD0))
    1 ((hit-key VK_NUMPAD1))
    2 ((hit-key VK_NUMPAD2))
    3 ((hit-key VK_NUMPAD3))
    4 ((hit-key VK_NUMPAD4))
    5 ((hit-key VK_NUMPAD5))
    6 ((hit-key VK_NUMPAD6))
    7 ((hit-key VK_NUMPAD7))
    8 ((hit-key VK_NUMPAD8))
    9 ((hit-key VK_NUMPAD9))))

(defn type-key [key]
  (. robot (keyPress ~key))
  (. robot (keyRelease ~key)))

(defn type-key-alt [^String digits]
    (. robot keyPress (. KeyEvent VK_ALT))
    (doseq [i (range 0 (count digits))]
      (type-numpad (Integer/parseInt (. digits (substring i (+ i 1))))))
    (. robot keyRelease (. KeyEvent VK_ALT)))

(def use-alt-hack (. (. System getProperty "os.name") startsWith "Windows"))

(defn type-char [^String s]
  (if use-alt-hack
    (cond
       (= s "\u00e4") (type-key-alt "132") ;ä
       (= s "\u00f6") (type-key-alt "148") ;ö
       (= s "\u00fc") (type-key-alt "129") ;ü
       (= s "\u00c4") (type-key-alt "142") ;Ä
       (= s "\u00d6") (type-key-alt "153") ;Ö
       (= s "\u00dc") (type-key-alt "154") ;Ü
       (= s "\u00df") (type-key-alt "225") ;ß
       :else (let [code-point (. s (codePointAt 0))
		   unicode-digits (String/valueOf code-point)]
	       (type-key-alt unicode-digits)))
    (cond
     (= s "a") ((hit-key VK_A))
     (= s "b") ((hit-key VK_B))
     (= s "c") ((hit-key VK_C))
     (= s "d") ((hit-key VK_D))
     (= s "e") ((hit-key VK_E))
     (= s "f") ((hit-key VK_F))
     (= s "g") ((hit-key VK_G))
     (= s "h") ((hit-key VK_H))
     (= s "i") ((hit-key VK_I))
     (= s "j") ((hit-key VK_J))
     (= s "k") ((hit-key VK_K))
     (= s "l") ((hit-key VK_L))
     (= s "m") ((hit-key VK_M))
     (= s "n") ((hit-key VK_N))
     (= s "o") ((hit-key VK_O))
     (= s "p") ((hit-key VK_P))
     (= s "q") ((hit-key VK_Q))
     (= s "r") ((hit-key VK_R))
     (= s "s") ((hit-key VK_S))
     (= s "t") ((hit-key VK_T))
     (= s "u") ((hit-key VK_U))
     (= s "v") ((hit-key VK_V))
     (= s "w") ((hit-key VK_W))
     (= s "x") ((hit-key VK_X))
     (= s "y") ((hit-key VK_Y))
     (= s "z") ((hit-key VK_Z))
     (= s "A") ((hit-key-shift VK_A))
     (= s "B") ((hit-key-shift VK_B))
     (= s "C") ((hit-key-shift VK_C))
     (= s "D") ((hit-key-shift VK_D))
     (= s "E") ((hit-key-shift VK_E))
     (= s "F") ((hit-key-shift VK_F))
     (= s "G") ((hit-key-shift VK_G))
     (= s "H") ((hit-key-shift VK_H))
     (= s "I") ((hit-key-shift VK_I))
     (= s "J") ((hit-key-shift VK_J))
     (= s "K") ((hit-key-shift VK_K))
     (= s "L") ((hit-key-shift VK_L))
     (= s "M") ((hit-key-shift VK_M))
     (= s "N") ((hit-key-shift VK_N))
     (= s "O") ((hit-key-shift VK_O))
     (= s "P") ((hit-key-shift VK_P))
     (= s "Q") ((hit-key-shift VK_Q))
     (= s "R") ((hit-key-shift VK_R))
     (= s "S") ((hit-key-shift VK_S))
     (= s "T") ((hit-key-shift VK_T))
     (= s "U") ((hit-key-shift VK_U))
     (= s "V") ((hit-key-shift VK_V))
     (= s "W") ((hit-key-shift VK_W))
     (= s "X") ((hit-key-shift VK_X))
     (= s "Y") ((hit-key-shift VK_Y))
     (= s "Z") ((hit-key-shift VK_Z))
     (= s " ") ((hit-key-shift VK_SPACE))
     (= s "1") ((hit-key VK_1))
     (= s "2") ((hit-key VK_2))
     (= s "3") ((hit-key VK_3))
     (= s "4") ((hit-key VK_4))
     (= s "5") ((hit-key VK_5))
     (= s "6") ((hit-key VK_6))
     (= s "7") ((hit-key VK_7))
     (= s "8") ((hit-key VK_8))
     (= s "9") ((hit-key VK_9))
     (= s "0") ((hit-key VK_0))
     (= s "!") ((hit-key-shift VK_1))
     (= s "@") ((hit-key-shift VK_2))
     (= s "#") ((hit-key-shift VK_3))
     (= s "$") ((hit-key-shift VK_4))
     (= s "%") ((hit-key-shift VK_5))
     (= s "^") ((hit-key-shift VK_6))
     (= s "&") ((hit-key-shift VK_7))
     (= s "*") ((hit-key-shift VK_8))
     (= s "(") ((hit-key-shift VK_9))
     (= s ")") ((hit-key-shift VK_0))
     (= s ",") ((hit-key VK_COMMA))
     (= s ".") ((hit-key VK_PERIOD))
     (= s "/") ((hit-key VK_SLASH))
     (= s ";") ((hit-key VK_SEMICOLON))
     (= s "'") ((hit-key VK_QUOTE))
     (= s "[") ((hit-key VK_OPEN_BRACKET))
     (= s "]") ((hit-key VK_CLOSE_BRACKET))
     (= s "\\") ((hit-key VK_BACK_SLASH))
     (= s "-") ((hit-key VK_MINUS))
     (= s "=") ((hit-key VK_EQUALS))
     (= s "<") ((hit-key-shift VK_COMMA))
     (= s ">") ((hit-key-shift VK_PERIOD))
     (= s "?") ((hit-key-shift VK_SLASH))
     (= s ":") ((hit-key-shift VK_SEMICOLON))
     (= s "\"") ((hit-key-shift VK_QUOTE))
     (= s "{") ((hit-key-shift VK_OPEN_BRACKET))
     (= s "}") ((hit-key-shift VK_CLOSE_BRACKET))
     (= s "|") ((hit-key-shift VK_BACK_SLASH))
     (= s "_") ((hit-key-shift VK_MINUS))
     (= s "+") ((hit-key-shift VK_EQUALS))
     :else (println "Unkown key: " s))))

(defn type-string [^String s]
  (try
    (cond
     (= s "BK") ((hit-key VK_BACK_SPACE))
     (= s "EN") ((hit-key VK_ENTER))
     :else (doseq [c s] (type-char (str c))))
  (catch IllegalArgumentException e
    (.. System out (println (string/join "" ["I could not type " s ". Is your current keyboard layout en-us?"]))))))


(defmacro hit-key-c [keycode] `(fn [] (. robot (keyPress ~keycode))
                                (. robot (keyRelease ~keycode))))

(defmacro typed-key [s] (let [ stroke (KeyStroke/getKeyStroke (str "pressed " s))
                                    code (. stroke getKeyCode)]
                                `(fn [] (. robot (keyPress ~code))
                                        (. robot (keyRelease ~code)))))

(defmacro char-item [c] `(MenuLeaf. ~c (fn [] (type-string ~c))))

;(def the-menu
;  (clojure.walk/postwalk (fn [x] (if (string? x) (char-item x) x))
;  [
;    [ " " "." "enter" "?" " " "!" ":" ","]
;    [ "\"" "'" "-" " " " " " " " " " "]
;    [ " " "a" "b" "c" "d" " " " " " "]
;    [ "[" "]" "e" "f" "g" "h" "{" "}"]
;    [ " " ")" "i" "j" "bksp" "k" "l" "("]
;    [ " " " " " " "m" "n" "o" "p" "q"]
;    [ " " " " " " "r" "s" "t" "u" "v"]
;    [ " " " " " " " " "w" "x" "y" "z"]
;  ]))

;(def the-menu
;  (clojure.walk/postwalk (fn [x] (if (string? x) (char-item x) x))
;[
;  [
;    [ "en" "t" "v" "7" " " "6" "z" "u"]
;    [ " " "," "h" "\u00fc" "9" " " "8" "\u00f6"]
;    [ "q" "s" "." "r" "p" "\u0060" "\u00df" "\u00b4"]
;    [ " " ";" "n" "\"" "m" ":" "\u005e" " "]
;    [ " " ")" "\u00e4" "a" "bk" "c" "b" "("]
;    [ "1" " " "0" "f" "e" "'" "d" "g"]
;    [ "k" "3" " " "2" "j" "i" "!" "l"]
;    [ "w" "y" "5" " " "4" "x" "o" "?"]
;  ]
;  [
;    [ "en" "T" "V" "7" " " "6" "Z" "U"]
;    [ " " "," "H" "\u00dc" "9" " " "8" "\u00d6"]
;    [ "Q" "S" "." "R" "P" "/" " " "\\"]
;    [ "}" ";" "N" "\"" "M" ":" "{" " "]
;    [ " " ")" "\u00c4" "A" "bk" "C" "B" "("]
;    [ "1" " " "0" "F" "E" "," "D" "G"]
;    [ "K" "3" " " "2" "J" "I" "!" "L"]
;    [ "W" "Y" "5" " " "4" "X" "O" "?"]
;  ]
;]))


(def the-menu
  (clojure.walk/postwalk (fn [x] (if (string? x) (char-item x) x))
[
  [
    [ "EN" "m" "v" "be" "tion" "at" "z" "a"]
    [ " " "," "l" "er" "re" "ould" "en" "ed"]
    [ "q" "s" "." "c" "p" "ha" "you" "on"]
    [ "in" ";" "e" "\"" "i" ":" "nd" "and"]
    [ "was" "to" "-" "r" "BK" "o" "b" "is"]
    [ "it" "ent" "as" "f" "h" "'" "d" "g"]
    [ "k" "es" "that" "ng" "j" "n" "!" "u"]
    [ "w" "y" "te" "ing" "ly" "x" "t" "?"]
  ]
  [
    [ "*" "M" "V" "+" "=" "" "Z" "A"]
    [ "en" "1" "L" "2" "3" "4" "5" "6"]
    [ "Q" "S" "7" "C" "P" "8" "9" "0"]
    [ "\u00e4" "\\" "E" "#" "I" "\u00df" "\u0060" "/"]
    [ "\u00b4" ")" "%" "R" "&" "O" "B" "("]
    [ "<" "|" ">" "F" "H" "_" "D" "G"]
    [ "K" "[" "\u00f6" "]" "J" "N" "" "U"]
    [ "W" "Y" "{" "\u00fc" "}" "X" "T" "$"]
  ]
]))



(defrecord AppState [pen-pos pen-down preview-x preview-y preview-scale menu-x menu-y menu-scale menu-active current-menu active-layer active-item may-be-tap recently-typed last-key])

; large preview
;(def the-state (ref (AppState. [0 0] false 100 100 80 270 100 16 false (the-menu 0) 0 nil)))
; standard
(def the-state (ref (AppState. [0 0] false 100 100 65 250 100 16 true (the-menu 0) 0 nil true "" "")))
; left-handed
;(def the-state (ref (AppState. [0 0] false 180 80 50 30 80 16 false (the-menu 0) 0 nil)))
; huge
;(def the-state (ref (AppState. [0 0] false 200 200 80 550 100 16 true (the-menu 0) 0 nil true "" "")))

;; Geometry

(def menu-circle-radius (math/cos (* (/ 3.0 8.0) PI)))

(defn menu-circle-center [i] [(math/cos (* i (/ PI 4))) (math/sin (* i (/ PI 4)))])

(def menu-circle-centers (for [i (range 0 8)] (menu-circle-center i)))

(defn inner-prod [p q] (reduce + (map (fn [pair] (* (pair 0) (pair 1))) (map vector p q))))

(defn diff [p q] (map (fn [pair] (- (pair 0) (pair 1))) (map vector p q)))

(defn distance [p q]  (math/sqrt (inner-prod (diff p q) (diff p q))))

(defn win2menu [p] 
  (let [x (p 0)
        y (p 1)
        translated-x (- x (:menu-x @the-state))
        translated-y (- y (:menu-y @the-state))
        final-x (/ translated-x (:menu-scale @the-state))
        final-y (/ translated-y (:menu-scale @the-state))]
    [final-x final-y]))

(defn menu2win [p] 
  (let [x (p 0)
        y (p 1)
        scaled-x (* x (:menu-scale @the-state))
        scaled-y (* y (:menu-scale @the-state))
        final-x (+ scaled-x (:menu-x @the-state))
        final-y (+ scaled-y (:menu-y @the-state))]
    [final-x final-y]))

(defn menu2prev [p] 
  (let [x (p 0)
        y (p 1)
        scaled-x (* x (:preview-scale @the-state))
        scaled-y (* y (:preview-scale @the-state))
        final-x (+ scaled-x (:preview-x @the-state))
        final-y (+ scaled-y (:preview-y @the-state))]
    [final-x final-y]))



;; This is the entire application logic!

(defn xor [a b] (or (and a (not b)) (and (not a) b)))

(defn append-max [s1 s2]
  (let [result (string/join "" [s1 s2])]
    (str (string/reverse (string/take 5 (string/reverse result))))))

(defstatetrafo process-input the-state [p down]
  (:= 
    :pen-pos p 
    :pen-down down)
  (let [pen-down-just-now (and down (not (§ :pen-down)))
        pen-up-just-now (and (not down) (§ :pen-down))
        at-last-layer (= (§ :active-layer) (- (count the-menu) 1))]
    (if pen-up-just-now
      (do
	(:= :active-item nil :current-menu (the-menu (§ :active-layer)) :may-be-tap true)
	(if (§ :may-be-tap)
	  (if at-last-layer
	    (:= :active-layer 0 :current-menu (the-menu 0))
	    (:= :active-layer (inc (§ :active-layer)) :current-menu (the-menu (inc (§ :active-layer)))))))))
  (if down    
    (let [new-active-item (let [all (for [i (range 0 8) :when (< (distance (menu-circle-center i) p) menu-circle-radius)] i)]
			    (first all))
	  active-item-changed (not (= new-active-item (§ :active-item)))
	  item-just-activated (and active-item-changed (not (nil? new-active-item)))
	  the-item (if (and new-active-item (vector? (§ :current-menu)))
		     ((§ :current-menu) new-active-item)
		     nil)]
      (if (§ :menu-active)
	(do 
	  (:= :active-item new-active-item)	
	  (if item-just-activated
	    (do
	      (:= :may-be-tap false)
	      (cond 
	       (and the-item (= (type the-item) net.inkcode.inkboard.MenuLeaf))
	       (do
		 ((:action the-item))
		 (:= :recently-typed (append-max (§ :recently-typed) (str (:label the-item))) :last-key (str (:label the-item)))
		 (:= :active-layer 0 :current-menu (the-menu 0)))
	       (and the-item (vector? the-item))
	       (:= :current-menu the-item)))))))))




;; Drawing

(def old-x (ref 0))
(def old-y (ref 0))
(def old-down (ref false))

(def ^BufferedImage image-background (ImageIO/read (new File "images/inkboard-background.png")))
;(def ^BufferedImage image-background (ImageIO/read (new File "images/white.png")))

;; ring images are 256x256
;; * menu is centered
;; * radius of the menu is 80
(def image-menu-active (ImageIO/read (new File "images/radial-menu-ring.png")))
(def image-menu-highlight
     [(ImageIO/read (new File "images/radial-menu-ring-0.png"))
      (ImageIO/read (new File "images/radial-menu-ring-1.png"))
      (ImageIO/read (new File "images/radial-menu-ring-2.png"))
      (ImageIO/read (new File "images/radial-menu-ring-3.png"))
      (ImageIO/read (new File "images/radial-menu-ring-4.png"))
      (ImageIO/read (new File "images/radial-menu-ring-5.png"))
      (ImageIO/read (new File "images/radial-menu-ring-6.png"))
      (ImageIO/read (new File "images/radial-menu-ring-7.png"))
      ])

;; draw radial menu from one of the above image files such that
;; (x,y) is the menu center and r its radius
;; performs scaling on the fly!
(defn draw-menu-image [^Graphics2D g img x y r]
  (let [s (* r (/ 128 80))]
    (prof/prof :image
	       (. g drawImage img (- x s) (- y s) (+ x s) (+ y s) 0 0 256 256 nil)))
  )

(defn radius-to-s [r] (* r (/ 128 80)))
  
(defn scale-menu-image [img r]
  (let [s (radius-to-s r)
	w (* 2 s)
	tmp (new BufferedImage w w (. BufferedImage TYPE_INT_ARGB))
	^Graphics2D g (. tmp createGraphics)]
    (. g setRenderingHint (. RenderingHints KEY_INTERPOLATION) (. RenderingHints VALUE_INTERPOLATION_BICUBIC))
    (. g setRenderingHint (. RenderingHints KEY_ANTIALIASING) (. RenderingHints VALUE_ANTIALIAS_ON))
    (. g drawImage img 0 0 w w nil)
    (. g dispose)
    tmp))

(def image-menu-active-small (scale-menu-image image-menu-active (:menu-scale @the-state)))
(def image-menu-active-large (scale-menu-image image-menu-active (:preview-scale @the-state)))
(def image-menu-highlight-small (map (fn [img] (scale-menu-image img (:menu-scale @the-state))) image-menu-highlight))
(def image-menu-highlight-large (map (fn [img] (scale-menu-image img (:preview-scale @the-state))) image-menu-highlight))

(defn draw-menu-images [^Graphics2D g]
  (let [item (:active-item @the-state)
	^BufferedImage large-image (if (not (nil? item))
		      (nth image-menu-highlight-large item)
		      image-menu-active-large)
	^BufferedImage small-image (if (not (nil? item))
		      (nth image-menu-highlight-small item)
		      image-menu-active-small)
	mx (:menu-x @the-state)
	my (:menu-y @the-state)
	mr (:menu-scale @the-state)
	ms (radius-to-s mr)
	px (:preview-x @the-state)
	py (:preview-y @the-state)
	pr (:preview-scale @the-state)
	ps (radius-to-s pr)]
    (. g drawImage small-image (int (- mx ms)) (int (- my ms)) nil)
    (. g drawImage large-image (int (- px ps)) (int (- py ps)) nil)))
    
    
(defn draw-circle [^Graphics2D g p r f]
  (let [^Ellipse2D$Double circ (new Ellipse2D$Double (- (p 0) r) (- (p 1) r)  (* 2 r) (* 2 r))]
    (if f
      (. g fill circ)
      (. g draw circ))))

(defn draw-text-centered [^Graphics2D g ^String text]
  (let [org-trans (. g getTransform)]
     ;(draw-circle g [0 0] 1.0 true)
     ;; the following code will position the str centrally on its disk
     ;; taking the fontAscent into account. Assumes that in the current
     ;; transformation, the center of the disk is at (0,0)
     (let [fm (. g getFontMetrics (new Font "Calibri" (. Font BOLD) 10))
	   rect (. fm getStringBounds text g)
	   height (. rect getHeight)
	   width (. rect getWidth)
	   ascent (. fm getAscent)]
       (. g translate (double (- (/ width 2))) (double (+ (- (/ height 2)) ascent))))
     (. g drawString text 0 0)
     (. g setTransform org-trans)))


(defn draw-text-right [^Graphics2D g ^String text]
  (let [org-trans (. g getTransform)]
     ;(draw-circle g [0 0] 1.0 true)
     (let [fm (. g getFontMetrics (new Font "Calibri" (. Font BOLD) 10))
	   rect (. fm getStringBounds text g)
	   height (. rect getHeight)
	   width (. rect getWidth)
	   ascent (. fm getAscent)]
       (. g translate (double (- width)) (double (+ (- (/ height 2)) ascent))))
     (. g drawString text 0 0)
     (. g setTransform org-trans)))


(defn draw-menu-text [^Graphics2D g menu]
  (let [org-trans (. g getTransform)]
    (cond
     (vector? menu)
     (doseq [i (range 0 8)]
       (let [v (prof/prof :mcc (menu-circle-center i))]
	 (. g setTransform org-trans)
	 (. g translate (double (v 0)) (double (v 1)))
	 (. g scale (double 0.27) (double 0.27))
	 (draw-menu-text g (menu i))))
     (= (type menu) net.inkcode.inkboard.MenuLeaf)
     (let [text (str (:label menu))
	   length (count text)
	   factor (math/pow 0.85 (dec length))	 ]
       (. g scale 0.25 0.25)
       (. g scale factor factor)
       (draw-text-centered g text))
     :else (.. System out (println ".")))
    (. g setTransform org-trans)))

(defn draw [^Graphics2D g] 
  (let [default-transform (. g getTransform)
 	draw-circles (fn [x y scale]                       
                       (. g translate (double x) (double y))
                       (. g scale (double scale) (double scale))
                       (doseq [i (range 0 8)]
                         (cond
                           (not (:menu-active @the-state)) (. g (setColor (. Color gray)))
                           (= i (:active-item @the-state)) (. g (setColor (. Color red)))
                           :else (. g (setColor (. Color blue))))
                         (draw-circle g (menu-circle-center i) menu-circle-radius true)))]
    (. g drawImage image-background (int 0) (int 0) nil)
    (draw-menu-images g)
    (draw-circle g [@old-x @old-y] 1.5 true)
    (if (:menu-active @the-state)
      (do
	(. g setRenderingHint (. RenderingHints KEY_ANTIALIASING) (. RenderingHints VALUE_ANTIALIAS_ON))
	(. g (setTransform default-transform))
	(. g (setColor (. Color black )))
	(. g (setFont (new Font "Calibri" (. Font BOLD) 10)))
	(. g translate (double (:preview-x @the-state)) (double (:preview-y @the-state)))
	(. g scale (double (:preview-scale @the-state)) (double (:preview-scale @the-state)))
	(draw-menu-text g (:current-menu @the-state))
	(. g setTransform default-transform)
	(. g translate (double (:preview-x @the-state)) (double (:preview-y @the-state)))
	(. g scale (double (* 0.035 (:preview-scale @the-state))) (double (* 0.035 (:preview-scale @the-state))))
	(. g setColor (. Color white))
	(draw-text-centered g (:recently-typed @the-state))
	(. g (setTransform default-transform))
	(. g (setColor (. Color red)))
	(draw-circle g (menu2prev (:pen-pos @the-state)) 3.0 true)
	))
      ))

;; Plumbing

      
(defn go []
  (let [frame (new JFrame)
        panel (proxy [JPanel] []
                ;(update [g] (.paint g))
                (paintComponent [^Graphics g]
                  ;(proxy-super paintComponent g)
                  ;(.. System out (println (:pen-x @the-state)))
                  (draw g)))
        timer (new Timer 20 (proxy [ActionListener] []
                (actionPerformed [e]
                  (. frame repaint))))
        pen-manager (new PenManager panel)
        pen (. pen-manager pen)
        robot (new Robot)]
      (. pen (addListener
               (proxy [PenListener] []
                 (penLevelEvent [^PLevelEvent ev]
                   ;(.. System out (println ev))
                   (let [values (loop [levels (. ev levels) 
                                       values {:x @old-x  :y @old-y :d @old-down}]
                                  ;(.. System out (println values))
                                  (if (empty? levels)
                                    values
                                    ;(persistent! values)
                                    (let [^PLevel level (first levels)]
                                      (condp = (. level getType) 
                                        PLevel$Type/X (recur (rest levels) (assoc values 
                                                                             :x (. level value) ))
                                        PLevel$Type/Y (recur (rest levels) (assoc values 
                                                                             :y (. level value) ))
                                        PLevel$Type/PRESSURE (recur (rest levels) (assoc values 
                                                                                    :d (not (= 0.0 (. level value)))))
                                        (recur (rest levels) values)))))]
                     (dosync
                       (ref-set old-x (:x values))
                       (ref-set old-y (:y values))
                       (ref-set old-down (:d values)))
                     ;(.. System out (println values))
                     (let [new-pos (win2menu [@old-x @old-y])
                           new-down @old-down]
                       ;(.. System out (println new-pos))
                       (process-input new-pos new-down))
                     ;(. robot (keyPress (. KeyEvent VK_A)))
                     ;(. robot (keyRelease (. KeyEvent VK_A)))
                     ;(. frame repaint)
                     ))
                 (penButtonEvent [ev])
                 (penKindEvent [ev])
                 (penScrollEvent [ev])
                 (penTock [millis]))))
      ;(. panel setBorder (. BorderFactory createEmptyBorder 0 0 0 0))
      (. frame add panel)
      (. panel setPreferredSize (new Dimension 300 200))
      (. frame pack)
      ;(prn (. panel getSize))
      ;(prn (. panel getBounds))
      ;(prn (. frame getSize))
      ;; The following should not be necessary. For some reason there is a
      ;; padding of 5 pixels all around added to the size of the panel. See
      ;; http://stackoverflow.com/questions/1593683/unwanted-border-around-jpanel
      (let [p (. panel getSize)
	    f (. frame getSize)
	    delta-w (- (. f getWidth) (. p getWidth))
	    delta-h (- (. f getHeight) (. p getHeight))]
	(. frame setSize (- (+ 300 delta-w) 10) (- (+ 200 delta-h) 10)))
      (. frame (setTitle "InkBoard v0.5"))
      (. frame (setAlwaysOnTop true))
      (. frame (setResizable false))
      (. frame (setFocusable false))
      (. frame (setFocusableWindowState false))
      (let [pixels (int-array 256)
	    image (. (Toolkit/getDefaultToolkit) createImage (new MemoryImageSource 16 16 pixels 0 16))
	    cursor (. (Toolkit/getDefaultToolkit) createCustomCursor image (new Point 0 0) "invisibleCursor")]
	(. frame setCursor cursor))
      (. frame setIconImage (. (new ImageIcon "images/icon-16x16.png") getImage))
      (. frame
        (addWindowListener
          (proxy [WindowAdapter] []
            (windowClosing [event]
              (. System (exit 0))))))
      (. frame show)
      (. timer setCoalesce true)
      (. timer start)))

(defn -main [& args] (go))