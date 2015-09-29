(ns hamt.core)

(defn bits [num]
  (Integer/toBinaryString num))

;; make an empty hamt
;; assoc into the trie
;; get key from trie

;; data -stores data
;; children -- 0-31 vec with slots for subtries

;;{:data nil :children []}
(defn hamt
  ([] {:key nil :value nil :children (vec (repeat 31 nil))})
  ([key val] {:key key :value val :children (vec (repeat 31 nil))}))

;; {:children [{:key nil :value nil :children []} {...}]}

(defn h-assoc [trie key value & [hash-digits]]
  (if (nil? (:key trie))
    (assoc trie :key key :value value)
    (let [hash-digits (or hash-digits (hash key))
          i (bit-and 31 hash-digits)
          subtrie (get-in trie [:children i])]
      (if subtrie
        (assoc-in
         trie [:children i]
         (h-assoc subtrie
                  key
                  value
                  (bit-shift-right hash-digits 5)))
        (assoc-in
         trie
         [:children i]
         (hamt key value))))))

;; hashcodes -> positions in the children list
;; 32 children in each level, 5 bits of hashcode
;; to ID a position
;;
(defn h-get [trie key]
  (if (= key (:key trie))
    (:value trie)
    (let [h (hash key)
          i (bit-and 31 h)
          subtrie (get-in trie [:children i])]
      (if subtrie
        (h-get subtrie key)
        nil))))


(def compact (partial filter (comp not nil?)))
(defn trie-string [t]
  (if t
    (str "Key: " (:key t) " Val: " (:value t) " Sub: \n    "
         (clojure.string/join "\n  " (map trie-string (compact (:children t)))))
    ""))
