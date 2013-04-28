.. highlight:: cl
   :linenothreshold: 0

第八章：符號
***************************************************

符號我們已經用了好長一段時間。蘊藏在符號背後還有更多。起初或許最好不要糾結背後的實現機制。你可以像資料物件與名字那樣使用符號，而不需要理解兩者是如何相關聯的。但到了某個時候，停下來並思考背後是如何工作是很有用的。這個章節解釋了背後的細節。

8.1 符號名 (Symbol Names)
==================================

第二章描述過符號是用來作為變數名，而符號本身是以一個物件所存在。但 Lisp 符號的可能性，要比在多數語言僅允許作為變數名來得廣泛許多。實際上，一個符號可以用任何字串作為名稱。你可以透過呼叫 ``symbol-name`` 來獲得符號的名字：

::

	> (symbol-name 'abc)
	"ABC"

注意到這個符號的名字全部是大寫字母。預設情況下 Common Lisp 在讀入時，把符號名字所有的英文字母都轉成大寫。這表示 Common Lisp 預設是不分大小寫的：

::

	> (eql 'abc 'Abc)
	T
	> (CaR '(a b c))
	A

有一個特殊的語法用來參照符號，其名字包含空白或其他可能對於讀取器 (reader)重要的字元。任何存在垂直槓 (vertical bar)之間的字元序列 (sequence of characters)被視為一個符號。你可以這樣子在符號的名字中放入任何東西：

::

	> (list '|Lisp 1.5| '|| '|abc| '|ABC|)
	(|Lisp 1.5| || |abc| ABC)

當這樣的符號名讀入時，不會有大小寫轉換，而巨集字元和其他的字元被視為一般字元。

那什麼樣的符號不需要使用垂直槓來參照呢？基本上任何不是數字或不包含讀取器視為重要的字元的符號。一個快速找出你是否可以不用垂直槓來參照符號的方法，是看看 Lisp 如何印出它的。如果 Lisp 沒有用垂直槓表示一個符號，如上述列表的最後一個，那麼你也可以不用垂直槓。

記得垂直槓是一個特殊的語法來表示符號。它們不是符號的名字之一：

::

	> (symbol-name '|a b c|)
	"a b c"

(如果你想要在符號名使用垂直槓，你可以放一個反斜線在垂直槓的前面。)

譯註: 反斜線是 ``\`` (backslash)。

8.2 屬性列表 (Property Lists)
===============================

在 Common Lisp 裡，每一個符號都有一個屬性列表 (property-list) 或稱為 ``plist`` 。函數 ``get`` 接受一個符號及一個任何型別的鍵值，然後返回在符號的屬性列表中，與鍵值相關的數值：

::

	> (get 'alizarin 'color)
	NIL

它使用 ``eql`` 來比較各個鍵。若某個特定的屬性沒有找到時， ``get`` 返回 ``nil`` 。

要將一個值與一個鍵關聯起來時，你可以使用 ``setf`` 及 ``get`` :

::

	> (setf (get 'alizarin 'color) 'red)
	RED
	> (get 'alizarin 'color)
	RED

現在符號 ``alizarin`` 的 ``color`` 屬性是 ``red`` 。

.. figure:: ../images/Figure-8.1.png

**圖 8.1 符號的結構**

::

	> (setf (get 'alizarin 'transparency) 'high)
	HIGH
	> (symbol-plist 'alizarin)
	(TRANSPARENCY HIGH COLOR RED)

注意到屬性列表不以關聯列表 (assoc-lists)的形式表示，雖然他們用起來是一樣的。（譯註: 關聯列表在 3.14 節討論過）

在 Common Lisp 裡，屬性列表用得不多。他們大部分被雜湊表取代了 (4.8 小節)。

8.3 符號很不簡單 (Symbols Are Big)
=====================================

當我們輸入名字時，符號就被悄悄地產生出來了，而當它們被顯示時，我們只能看到符號的名字。某些情況下，把符號想成是我們所看到的東西就好，別想太多。但有時候符號不像看起來那麼簡單。

從我們如何使用及檢視符號，看起來符號像是整數那樣的小物件。實際上符號確實是一個物件，差不多像是由 ``defstruct`` 定義的那種結構。一個符號可以有名字、 ``home`` 套件 (package)、作為變數的值、作為函數的值以及一個屬性列表 (property list)。圖 8.1 展示了符號在內部是如何表示的。

很少有程式會使用很多符號，以致於值得用其它的來代替符號以節省空間。但是值得銘記在心的是，符號是實際的物件，不只是名字而已。當兩個變數設成相同的符號時，與兩個變數設成相同列表一樣：兩個變數都有指標指向同樣的物件。

8.4 創造符號 (Creating Symbols)
===================================================

8.1 節示範了如何從符號獲得它的名字。另一方面，從字串獲得符號也是有可能的。這比較複雜一點，因為我們需要介紹套件 (package)這個議題。

概念上套件是將名字映射到符號的符號表 (symbol-tables)。每一個普通的符號都屬於一個特定的套件。一個符號屬於一個套件，我們稱為符號被套件 interned 了。函數與變數用符號作為它們的名字。套件藉由限制哪個符號可以存取來實施模組化 (modularity)，也是因為這樣，我們才可以參照到函數與變數。

大多數的符號在讀取時被 interned 了。在你第一次輸入一個新符號的名字時，Lisp 會產生一個新的符號物件，並將它 intern 到當下的套件裡（預設是 ``common-lisp-user`` 套件)。但你也可以透過給入一個字串與選擇性套件參數 (optional package argument)給 ``intern`` 函數來 intern 一個符號:

::

	> (intern "RANDOM-SYMBOL")
	RANDOM-SYMBOL
	NIL

套件參數預設是當前的套件，所以前述的表達式，回傳當前套件裡的一個符號，此符號的名字是 "RANDOM-SYMBOL"，若此符號尚未存在時，會創造一個這樣的符號出來。第二個回傳值告訴我們符號是否存在；在這個情況，它不存在。

不是所有的符號都會被 interned。有時候有一個 uninterned 符號是有用的，這和有未公開的電話是一樣的原因。Uninterned 符號叫做 *gensyms* 。我們將會在第 10 章討論巨集 (Macro)時，理解 gensym 的作用。

8.5 多重套件 (Multiple Packages)
=======================================

大的程式通常分割成多個套件。如果程式的每一部分都是一個套件，那麼開發程式另一個部分的某個人，將可以使用符號來作為函數名或變數名，而不用擔心名字在別的地方已經被用到了。

在沒有提供定義多個命名空間的語言裡，工作於大專案的程式設計師，通常需要想出某些規範 (convention)來確保他們不會使用同樣的名稱。舉例來說，程式設計師寫顯示用的程式碼 (display code)可能用 ``disp_`` 開頭的名字，而寫數學程式 (math code)的程式設計師僅使用由 ``math_`` 開始的程式碼。所以若是數學程式裡包含一個函數來做快速傅立葉轉換 (fast Fourier transform)時，可能會叫做 ``math_fft`` 。

套件只不過是提供了一種方式來自動辦到這件事。如果你將函數定義在單獨的套件裡，你可以隨意使用你喜歡的名字。只有你顯式 ``export`` 的符號會被別的套件看到，而他們通常前面會有套件的名字(或修飾名)。

舉例來說，假設一個程式分成兩個套件， ``math`` 與 ``disp`` 。如果符號 ``fft`` 被 ``math`` 套件輸出，則 ``disp`` 套件裡可以用 ``math:fft`` 來參照它。在 ``math`` 套件裡，可以只用 ``fft`` 來參照。

這裡是你或許會放在檔案最上方，包含獨立套件的程式碼:

::

	(defpackage "MY-APPLICATION"
	            (:use "COMMON-LISP" "MY-UTILITIES")
	            (:nicknames "APP")
	            (:export "WIN" "LOSE" "DRAW"))

	(in-package my-application)

``defpackage`` 定義一個新的套件叫做 ``my-application`` [1]_ 它使用了其他兩個套件， ``common-lisp`` 與 ``my-utilities`` ，這代表著可以不需要用套件修飾符 (package qualifiers)來存取這些套件所輸出的符號。許多套件會使用 ``common-lisp`` –– 因為你不會想給 Lisp 內建的運算元與變數加上修飾符。

``my-application`` 套件本身只輸出三個符號: ``WIN`` , ``LOSE`` 以及 ``DRAW`` 。由於呼叫 ``defpackage`` 給了 ``my-application`` 一個匿稱 ``app`` ，別的套件的程式可以這樣參照到這些符號，比如 ``app:win`` 。

``defpackage`` 伴隨著一個 ``in-package`` ，確保當前套件是 ``my-application`` 。所有其它未修飾的符號會被 interned 至 ``my-application`` –– 除非之後有別的 ``in-package`` 。當一個檔案被載入時，當前的套件總是被重置成載入之前的值。

8.6 關鍵字 (Keywords)
=======================================

在 ``keyword`` 套件的符號 (稱為關鍵字)有兩個獨特的性質：它們總是對自己求值，以及你可以在任何地方參照它們，如 ``:x`` 而不是 ``keyword:x`` 。我們首次在 44 頁 (譯註: 3.10 小節）介紹關鍵字參數時， ``(member '(a) '((a) (z)) test: #'equal)`` 比 ``(member '(a) '((a) (z)) :test #'equal)`` 讀起來更自然。現在我們知道為什麼第二個較彆扭的形式才是對的。 ``test`` 前的冒號字首，是用來識別這是一個關鍵字。

為什麼使用關鍵字而不用一般的符號？因為他們在哪都可以存取。一個函數接受符號作為參數，應該要寫成預期關鍵字的函數。舉例來說，函數可以安全地在任何套件裡呼叫:

::

	(defun noise (animal)
	  (case animal
	    (:dog :woof)
	    (:cat :meow)
	    (:pig :oink)))

如果是用一般符號寫成的話，它只會在被定義的套件內工作，除非關鍵字也被輸出 (exported)了。

8.7 符號與變數 (Symbols and Variables)
=======================================

Lisp 有一個可能會困惑你的事情是，符號與變數的從兩個非常不同的層面互相關聯。當一個符號是一個特別變數 (special variable)的名字時，變數的值存在符號的 value 欄位 (圖 8.1)。 ``symbol-value`` 函數參照到那個欄位，所以在符號與特殊變數的值之間，有一個直接的連接 (connection)。

而對於詞法變數 (lexical variables)來說，事情就完全不一樣了。一個作為詞法變數的符號只是一個佔位符 (placeholder)。編譯器會將其轉為一個暫存器 (register)或記憶體位置的參照。在最後編譯出來的程式碼，我們無法追蹤這個符號 (除非它被除錯器「debugger」在某個地方保有著)。因此符號與詞法變數的值之間是沒有連接的；只要一有值，符號就消失了。

8.8 範例：隨機文字 (Example: Random Text)
=============================================

如果你要寫程式來操作單字，通常使用符號會比字串來得好，因為符號概念上是原子的 (atomic)。(譯註: 原子的意思即像一個原子是一個最小不可分割的單元。) 符號可以用 ``eql`` 一步比較完成，而字串需要使用 ``string=`` 或 ``string-equal`` 逐一字元做比較。作為一個例子，本節演示如何寫一個程式來產生隨機文字。程式的第一部分會讀入一個範例文件 (越大越好)，用來累積之後所給入的相關單字的可能性 (likeilhood)的資訊。第二部分在每一個單字都根據原本的範例，產生一個隨機的權重 (weight)之後，隨機走訪根據第一部分所產生的網路。

產生的文字將會是部分可信的 (locally plausible)，因為任兩個出現的單字也是輸入文件裡，兩個同時出現的單字。令人驚訝的是你可以頻繁地獲得看起來是 –– 有意義的整句 –– 甚至整個段落。

圖 8.2 包含了程式的上半部，用來讀取範例文件的程式碼。

::

	(defparameter *words* (make-hash-table :size 10000))

	(defconstant maxword 100)

	(defun read-text (pathname)
	  (with-open-file (s pathname :direction :input)
	    (let ((buffer (make-string maxword))
	          (pos 0))
	      (do ((c (read-char s nil :eof)
	              (read-char s nil :eof)))
	          ((eql c :eof))
	        (if (or (alpha-char-p c) (char= c #\'))
	            (progn
	              (setf (aref buffer pos) c)
	              (incf pos))
	            (progn
	              (unless (zerop pos)
	                (see (intern (string-downcase
	                               (subseq buffer 0 pos))))
	                (setf pos 0))
	              (let ((p (punc c)))
	                (if p (see p)))))))))

	(defun punc (c)
	  (case c
	    (#\. '|.|) (#\, '|,|) (#\; '|;|)
	    (#\! '|!|) (#\? '|?|) ))

	(let ((prev `|.|))
	  (defun see (symb)
	    (let ((pair (assoc symb (gethash prev *words*))))
	      (if (null pair)
	          (push (cons symb 1) (gethash prev *words*))
	          (incf (cdr pair))))
	    (setf prev symb)))

**圖 8.2 讀取範例文件**

從圖 8.2 導出的資料會被存在雜湊表 ``*words*`` 裡。這個雜湊表的鍵是代表單字的符號，而值會像是下列的關聯列表 (assoc-lists):

::

	((|sin| . 1) (|wide| . 2) (|sights| . 1))

使用\ `彌爾頓的失樂園 <http://zh.wikipedia.org/wiki/%E5%A4%B1%E6%A8%82%E5%9C%92>`_\ 作為範例文件時，這是與鍵 ``|discover|`` 有關的值。它指出了 “discover” 這個單字，在詩裡面用了四次，與 “wide” 用了兩次，而 “sin” 與 ”sights” 各一次。(譯註: 詩可以在這裡找到 http://www.paradiselost.org/ )

函數 ``read-text`` 累積了這個資訊。這函數接受一個路徑名 (pathname)，然後替每一個出現在文件中的單字，建立一個上面所展示的關聯列表。它的工作方式是每次讀取檔案的一個字元，將累積的單字存在字串 ``buffer`` 。有了 ``maxword=100`` ，程式可以讀取至多至 100 個字的單字，對英語來說足夠了。

只要下個字元是一個字 (由 ``alpha-char-p`` 決定)或是一撇 (apostrophe)，就持續累積字元。任何使單字停止累積的字元會送給 ``see`` 。數種標點符號 (punctuation)也被認為是單字；函數 ``punc`` 回傳標點字元的偽單字 (pseudo-word)。

函數 ``see`` 註冊每一個我們看過的字。它需要知道前一個單字以及我們剛確認過的單字 –– 這也是為什麼要有變數 ``prev`` 。起初這個變數設為偽單字裡的句點；在 ``see`` 函數被呼叫後， ``prev`` 變數包含了我們最後見過的單字。

在 ``read-text`` 回傳之後， *words* 會包含一個給輸入檔案的每一個單詞的條目 (entry)。透過呼叫 ``hash-table-count`` 你可以了解有多少個不同的單字存在。很少英文文件會超過 10000 個單字。

現在來到了有趣的部份。圖 8.3 包含了從圖 8.2 所累積的資料來產生文字的程式碼。 ``generate-text`` 函數導出整個過程。它接受一個要產生幾個單字的數字，以及選擇性傳入前一個單字。使用預設值，會讓產生出來的文件從句子的開頭開始。

::

	(defun generate-text (n &optional (prev '|.|))
	  (if (zerop n)
	      (terpri)
	      (let ((next (random-next prev)))
	        (format t "~A " next)
	        (generate-text (1- n) next))))

	(defun random-next (prev)
	  (let* ((choices (gethash prev *words*))
	         (i (random (reduce #'+ choices
	                            :key #'cdr))))
	    (dolist (pair choices)
	      (if (minusp (decf i (cdr pair)))
	          (return (car pair))))))

**圖 8.3 產生文字**

要取得一個新的單詞， ``generate-text`` 使用前一個單詞調用 ``random-next`` 。這個函數隨機選擇伴隨輸入文本中 ``prev`` 之後的單詞，根據每個單詞出現的機率加上權重。

現在會是讓程式來測試運行的好時機。但其實你已經看過一個它所產生的例子: 本書開頭的那首詩，是使用彌爾頓的失樂園作為輸入文件所產生的。

(譯註: 詩在這裡或是書的第 vi 頁)

Half lost on my firmness gains more glad heart,

Or violent and from forage drives

A glimmering of all sun new begun

Both harp thy discourse they match'd,

Forth my early, is not without delay;

For their soft with whirlwind; and balm.

Undoubtedly he scornful turn'd round ninefold,

Though doubled now what redounds,

And chains these a lower world devote, yet inflicted?

Till body or rare, and best things else enjoy'd in heav'n

To stand divided light at ev'n and poise their eyes,

Or nourish, lik'ning spiritual, I have thou appear.

–– Henley

Chapter 8 總結 (Summary)
============================

1. 一個符號的名字可以是任何字串，但由 ``read`` 創造的符號預設會被轉成大寫。

2. 符號有相關聯的屬性列表，雖然他們不需要是同樣的形式，但行為像是 assoc-lists 。

3. 符號是實質的物件，比較像結構而不是名字。

4. 套件將字串映射至符號。要在套件裡給符號創造一個條目的方法是 intern 它。符號不需要被 interned。

5. 套件通過限制可以參照的名稱增加模組化。預設你的套件會是 user 套件，但為了提高模組化，大的程式通常分成數個套件。

6. 可以讓符號在別的套件被存取。關鍵字是自身求值並在所有的套件裡都可以存取。

7. 當一個程式用來操作單字時，用符號來表示單字是很方便的。

Chapter 8 練習 (Exercises)
==================================

1. 可能有兩個符號有同樣的名字，但是不 ``eql`` 嗎？

2. 估計一下用字串表示 "FOO" 與符號表示 foo 所使用記憶體空間的差異。

3. 137 頁的 ``defpackage`` 呼叫只使用字串作為參數。我們應該使用符號。為什麼使用字串可能比較危險呢？

4. 加入需要的程式碼，使圖 7.1 的程式碼可以放在一個叫做 ``"RING"`` 的套件裡，而圖 7.2 的程式碼放在一個叫做 ``"FILE"`` 套件裡。你不需要更改現有的代碼。

5. 寫一個可以確認引用的句子是否由 Henley 是否程式 (8.8 節)。

6. 寫一個 Henley，可以接受一個單字，並產生一個包含該單字於中間的句子的版本。


.. rubric:: 腳註

.. [1] 呼叫 ``defpackage`` 裡的名字全部大寫是因為在 8.1 節提到過，符號的名字預設被轉成大寫。