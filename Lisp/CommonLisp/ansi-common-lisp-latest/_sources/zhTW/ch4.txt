.. highlight:: cl
   :linenothreshold: 0

第四章：特殊資料結構
***************************************************

在之前的章節裡，我們討論了列表，Lisp 最多功能的資料結構。本章展示如何使用 Lisp 其它的資料結構：陣列（包含向量與字串），結構以及雜湊表。他們或許不像列表這麼彈性，但他們可以更快地存取並使用更少空間。

Common Lisp 有另一個資料結構：實體（instance）。實體在 11 章討論，講述 CLOS。

4.1 陣列 (Array)
===================

在 Common Lisp 裡，你可以呼叫 ``make-array`` 創建一個陣列，第一個參數為一個列表，指定陣列的維度。要創建一個 ``2 x 3`` 的陣列我們可以：

::

  > (setf arr (make-array '(2 3) :initial-element nil))
  #<Simple-Array T (2 3) BFC4FE>

陣列在 Common Lisp 裡至少可以有七個維度，每個維度至少可以有 1023 個元素。

``:initial-element`` 參數是選擇性的。如果提供了這個參數，整個陣列會用指定的值作初始化。嘗試從一個未初始化的陣列內取出元素的行為，其結果為未定義（undefined）。

取出陣列內的元素我們呼叫 ``aref`` 。跟 Common Lisp 的存取函數相同， ``aref`` 是零索引的（zero-indexed）：

::

  > (aref arr 0 0)
  NIL

要替換陣列的某個元素，我們使用 ``setf`` 和 ``aref`` ：

::

  > (setf (aref arr 0 0) 'b)
  B
  > (aref arr 0 0)
  B

要表示一個字面陣列（literal array），我們使用 ``#na`` 語法，其中 n 是陣列的維度。舉例來說，我們可以這樣表示一個跟 ``arr`` 一樣的陣列：

::

  #2a((b nil nil) (nil nil nil))

如果全域變數 ``*print-array*`` 為真，陣列會用這種形式顯示：

::

  > (setf *print-array* t)
  T
  > arr
  #2A((B NIL NIL) (NIL NIL NIL))

如果我們只想要一個一維的陣列，你可以給 ``make-array`` 第一個參數一個整數，而不是一個列表：

::

  > (setf vec (make-array 4 :initial-elment nil))
  #(NIL NIL NIL NIL)

一個一維陣列又稱為向量( *vector* )。你可以用一個步驟來創建及填滿向量，藉由呼叫 ``vector`` ，它會回傳一個無論你給入什麼參數的向量：

::

  > (vector "a" 'b 3)
  #("a" b 3)

一個字面向量(literal vector)可以用這種語法表達，如同字面陣列可以表示成 ``#na`` 。

你可以用 ``aref`` 來存取向量，但有一個更快的函數叫做 ``svref`` 給存取向量使用。

::

  > (svref vec 0)
  NIL

在 ``svref`` 內的 "sv" 代表 "簡單向量" ("simple vector") ，它是所有向量的預設值。 [1]_

4.2 範例：二分搜索 (Example: Binary Search)
=============================================

做為一個範例，這小節展示如何寫一個在排序好的向量裡搜索一個物件的函數。如果我們知道一個向量是排序好的，我們可以比 ``find`` （65頁）做的更好， ``find`` 必須依序檢視每一個元素。取而代之的，我們跳到向量中間開始。如果中間的元素是我們要找的物件，搜索完畢。不然，我們持續往左半部或往右半部搜索，取決於物件是小於或大於中間的元素。

圖 4.1 包含了一個這樣工作的函數。其實這兩個函數： ``bin-search`` 設置初始範圍及發送控制信號給 ``finder`` ，它尋找向量 ``vec`` 內 ``obj`` 是否介於 ``start`` 及 ``end`` 之間。

::

  (defun bin-search (obj vec)
    (let ((len (length vec)))
      (and (not (zerop len))
           (finder obj vec 0 (- len 1)))))

  (defun finder (obj vec start end)
    (let ((range (- end start)))
      (if (zerop range)
          (if (eql obj (aref vec start))
              obj
              nil)
          (let ((mid (+ start (round (/ range 2)))))
            (let ((obj2 (aref vec mid)))
              (if (< obj obj2)
                  (finder obj vec start (- mid 1))
                  (if (> obj obj2)
                      (finder obj vec (+ mid 1) end)
                      obj)))))))

圖 4.1: 搜索一個排序好的向量

**勘誤:** 圖 4.1 的 ``bin-search`` 函數，如果你給一個比向量 ``vec`` 中最小元素還小的元素，會出錯。

如果要找的 ``range`` 縮小至一個元素，而如果這個元素是 ``obj`` 的話，則 ``finder`` 回傳這個元素，反之回傳 ``nil`` 。如果 ``range`` 包含了數個元素，我們比對 ``middle`` ( ``round`` 回傳離參數最近的整數) 與 ``obj2`` 。如果 ``obj`` 小於 ``obj2`` ，則繼續遞迴地往向量的左半部尋找。如果 ``obj`` 大於 ``obj2`` ，則繼續遞迴地往向量的右半部尋找。剩下的一個選擇是 ``obj=obj2`` ，這個情況我們找到要找的元素，直接回傳這個元素。

如果我們插入下面這行至 ``finder`` 的起始處：

::

  (format t "~A~%" (subseq vec start (+ end 1)))

我們可以觀察被搜索的元素的數量是每一步往左減半的：

::

  > (bin-search 3 #(0 1 2 3 4 5 6 7 8 9))
  #(0 1 2 3 4 5 6 7 8 9)
  #(0 1 2 3)
  #(3)
  3


4.3 字元與字串 (Strings and Characters)
=============================================

字串是字元向量。我們用一系列由雙引號包住的字元來表示一個字串常數，一個字元 ``c`` 用 ``#\c`` 表示。

每個字元都有一個相關的整數 ― 通常是用ASCII碼，但不一定是。在多數的 Lisp 實現裡，函數 ``char-code`` 回傳與字元相關的數字，而 ``code-char`` 回傳與數字相關的字元。

字元比較函數 ``char<`` (小於)， ``char<=`` (小於等於)， ``char=`` (等於)， ``char>=`` (大於等於)， ``char>`` (大於)，以及 ``char/=`` (不同)。他們的工作方式和 146 頁(譯註 9.3 節)的數字比較運算元一樣。

::

  > (sort "elbow" #'char<)
  "below"

因為字串是向量，序列與陣列的函數都可以給字串使用。你可以使用 ``aref`` 來取出元素，舉例來說，

::

  > (aref "abc" 1)
  #\b

但對一個字串，你可以使用更快的 ``char`` 函數：

::

  > (char "abc" 1)
  #\b

你可以使用 ``setf`` 搭配 ``char`` (或 ``aref`` )來替換元素：

::

  > (let ((str (copy-seq "Merlin")))
      (setf (char str 3) #\k)
      str)

如果你想要比較兩個字串，你可以使用通用的 ``equal`` 函數，但還有一個忽略大小寫的比較函數 ``string-equal`` ：

::

  > (equal "fred "fred")
  T
  > (equal "fred" "Fred")
  NIL
  >(string-equal "fred" "Fred")
  T

Common Lisp 提供大量的操控及比較字串的函數。他們收錄在附錄D，從 364 頁開始。

有很多種方式可以創造一個字串。最普遍的方式是使用 ``format`` 。將第一個參數設為 ``nil`` 來呼叫 ``format`` ，使它回傳一個它本來會印出來的字串：

::

  > (format nil "~A or ~A" "truth" "dare")
  "truth or dare"

但若你只想把數個字串連結起來，你可以使用 ``concatenate`` ，它接受一個指定型別的符號，加上一個或多個序列：

::

   > (concatenate 'string "not " "to worry")
   "not to worry"

4.4 序列 (Sequences)
===========================

在 Common Lisp 裡，序列型別包含了列表與向量（因此也包含了字串）。有些我們在列表上使用的函數，其實是序列函數，包括 ``remove`` , ``length`` , ``subseq`` , ``reverse`` , ``sort`` , ``every`` 以及 ``some`` 。所以 46 頁 （譯註 3.11 的 ``mirror?`` 函數）我們所寫的函數，也可以用在別種序列上：

::

  > (mirror? "abba")
  T

我們已經看過四個用來取出序列元素的函數： 給列表使用的 ``nth`` ， 給向量使用的 ``aref`` 及 ``svref`` ，以及給字串使用的 ``char`` 。 Common Lisp 也提供了函數 ``elt`` ，對任何種類的序列都有效：

::

  > (elt '(a b c) 1)
  B

針對特定型別的序列，我們已經見過的存取函數應當比較快，所以使用 ``elt`` 是沒有意義的，除非在程式碼中，有要通用地支援序列的地方。

使用 ``elt`` ，我們可以寫一個對向量來說更有效率的 ``mirror?`` 版本：

::

  (defun mirror? (s)
    (let ((len (length s)))
      (and (evenp len)
           (do ((forward 0 (+ forward 1))
                (back (- len 1) (- back 1)))
               ((or (> forward back)
                    (not (eql (elt s forward)
                              (elt s back))))
                (> forward back))))))

這個版本也可以給列表使用，但這個實現更適合給向量使用。頻繁的對列表呼叫 ``elt`` 的代價是昂貴的，因為列表僅允許循序存取。而向量允許隨機存取，從任何元素來存取每一個元素都是廉價的 (cheap)。

許多序列函數接受一個或多個，從這個表格所列出的標準關鍵字參數：

+-----------+----------------------+-----------+
| 參數      | 用途                 | 預設值    |
+===========+======================+===========+
| :key      | 應用至每個元素的函數 | identity  |
+-----------+----------------------+-----------+
| :test     | 做為比較的函數       | eql       |
+-----------+----------------------+-----------+
| :from-end | 若為真，反向工作     | nil       |
+-----------+----------------------+-----------+
| :start    | 起始位置             | 0         |
+-----------+----------------------+-----------+
| :end      | 若有給定，結束位置。 | nil       |
+-----------+----------------------+-----------+

一個接受全部關鍵字參數的函數是 ``position`` ，它回傳序列中一個元素的位置，而未找到時，回傳 ``nil`` 。我們使用 ``position`` 來演示關鍵字參數所扮演的角色。

::

  > (position #\a "fantasia")
  1
  > (position #\a "fantasia" :start 3 :end 5)
  4

第二個例子我們要找在第四個與第六個字元間，第一個 ``a`` 所出現的位置。 ``:start`` 關鍵字參數是第一個被考慮的元素位置，預設是序列的第一個元素。 ``:end`` 關鍵字參數，如果有給的話，是第一個不被考慮的元素位置。

如果我們給入 ``:from-end`` 關鍵字參數，

::

  > (position #\a "fantasia" :from-end t)
  7

我們得到最靠近結尾的 ``a`` 的位置。但位置是用平常的方式計算；它不代表從結尾算回來的距離。

``:key`` 關鍵字參數是序列中每個元素在被考慮前，應用至元素的函數。如果我們詢問像是這樣的東西，

::

  > (position 'a '((c d) (a b)) :key #'car)
  1

那麼我們要找的是元素的 ``car`` 部分是符號 ``a`` 的第一個元素。

``:test`` 關鍵字參數是一個有兩個參數的函數，並定義了怎樣是一個成功的匹配。它的預設函數為 ``eql`` 。如果你想要匹配一個列表，你也許想使用 ``equal`` 來取代：

::

  > (position '(a b) '((a b) (c d)))
  NIL
  > (position '(a b) '((a b) (c d)) :test #'equal)
  0

``:test`` 關鍵字參數可以是任何接受兩個參數的函數。舉例來說，給定 ``<`` ，我們可以詢問第一個使第一個參數比它小的元素位置：

::

  > (position 3 '(1 0 7 5) :test #'<)
  2

使用 ``subseq`` 與 ``position`` ，我們可以寫出分開序列的函數。舉例來說，這個函數

::

  (defun second-word (str)
    (let ((p1 (+ (position #\  str) 1)))
      (subseq str p1 (position #\  str :start p1))))

回傳字串中用空格隔開的第二個單字：

::

  > (second-word "Form follows function")
  "follows"

要找到滿足接受一個參數的判斷式的一個元素，我們使用 ``position-if`` 。它接受一個函數與一個序列，並回傳第一個滿足此函數的第一個元素：

::

  > (position-if #'oddp '(2 3 4 5))
  1

它接受除了 ``:test`` 之外的所有關鍵字參數。

有許多相似的函數，如給序列使用的 ``member`` 與 ``member-if`` 。它們分別是， ``find`` （接受全部關鍵字參數）與 ``find-if`` （接受除了 ``:test`` 之外的所有關鍵字參數）：

::

  > (find #\a "cat")
  #\a

  > (find-if #'characterp "ham")
  #\h

不像是 ``member`` 與 ``member-if`` ，它們僅回傳要尋找的物件。

通常一個 ``find-if`` 的呼叫，如果解讀為 ``find`` 搭配一個 ``:key`` 關鍵字參數的話，會顯得更清楚。舉例來說，表達式

::

  (find-if #'(lambda (x)
               (eql (car x) 'complete))
           lst)

可以更好的解讀為

::

  (find 'complete lst :key #'car)

函數 ``remove`` (22頁)以及 ``remove-if`` 通常都可以用在序列。它們跟 ``find`` 與 ``find-if`` 是一樣的關係。一個相關的函數是 ``remove-duplicates`` ，它只保留序列中每個元素的最後一次出現。

::

  > (remove-duplicates "abracadabra")
  "cdbra"

這個函數接受前表所列的所有關鍵字參數。

函數 ``reduce`` 用來把一個序列壓縮成一個值。它接受至少兩個參數，一個函數與一個序列。這函數必須是一個接受兩個參數的函數。在最簡單的情況下，函數起初用前兩個元素做為參數來呼叫，之後接續的元素做為下次呼叫的第二個參數，而上次回傳的值做為下次呼叫的第一個參數。最後呼叫所回傳的值做為 ``reduce`` 函數的回傳值。也就是說像是這樣的表達式：

::

  (reduce #'fn '(a b c d))

等同於

::

  (fn (fn (fn 'a 'b) 'c) 'd)

我們可以使用 ``reduce`` 來擴充只接受兩個參數的函數。舉例來說，要得到三個或多個列表的交集 (intersection)，我們可以：

::

  > (reduce #'intersection '((b r a d 's) (b a d) (c a t)))
  (A)

4.5 範例：解析日期 (Example: Parsing Dates)
=============================================

做為一個序列操作的例子，這小節演示了如何寫一個程式來解析日期。我們將編寫一個程式，可以接受一個像是 "16 Aug 1980" 的字串，然後回傳一個表示日、月、年的整數列表。

::

  (defun tokens (str test start)
    (let ((p1 (position-if test str :start start)))
      (if p1
          (let ((p2 (position-if #'(lambda (c)
                                     (not (funcall test c)))
                                 str :start p1)))
            (cons (subseq str p1 p2)
                  (if p2
                      (tokens str test p2)
                      nil)))
          nil)))

  (defun constituent (c)
    (and (graphic-char-p c)
         (not (char= c #\ ))))

圖 4.2 辨別記號

圖 4.2 中包含了某些我們在這應用裡所需的通用解析函數。第一個， ``tokens`` ，用來從字串中取出記號(token)。給定一個字串及一個測試函數，它回傳一個字元滿足此函數的子字串的列表。舉例來說，如果測試函數是對字母回傳真的 ``alpha-char-p`` 函數，我們得到：

::

  > (tokens "ab12 3cde.f" #'alpha-char-p 0)
  ("ab" "cde" "f")

所有不滿足此函數的字元被視為空白 – 他們使記號分開，但永遠不是記號的一部分。

函數 ``constituent`` 被定義成用來做為 ``tokens`` 的參數。

在 Common Lisp 裡， *圖形字元* 是我們可見的字元，加上空白字元。所以如果我們用 ``constituent`` 做為測試函數時，

::

  > (tokens "ab12 3cde.f gh" #'constituent 0)
  ("ab12" "3cde.f" "gh")

則記號將會有一般常見的空白概念。

圖 4.3 包含了特別為解析日期用的函數。這函數 ``parse-date`` 接受一個特別形式的日期，並回傳一個代表其組成的整數列表：

::

  > (parse-date "16 Aug 1980")
  (16 8 1980)

::

  (defun parse-date (str)
    (let ((toks (tokens str #'constituent 0)))
      (list (parse-integer (first toks))
            (parse-month   (second toks))
            (parse-integer (third toks)))))

  (defconstant month-names
    #("jan" "feb" "mar" "apr" "may" "jun"
      "jul" "aug" "sep" "oct" "nov" "dec"))

  (defun parse-month (str)
    (let ((p (position str month-names
                           :test #'string-equal)))
      (if p
          (+ p 1)
          nil)))

圖 4.3 解析日期的函數

它使用 ``tokens`` 來解開一個日期字串，然後呼叫 ``parse-month`` 及 ``parse-integer`` 來解譯這些元素。要找到月份，它呼叫 ``parse-month`` ，由於使用的是 ``string-equal`` 來匹配月份的名字，所以輸入可以不分大小寫。要找到年和日，它呼叫內建的 ``parse-integer`` ， ``parse-integer`` 接受一個字串並回傳對應的整數。

如果我們需要寫程式來解析整數，我們也許可以：

::

  (defun read-integer (str)
    (if (every #'digit-char-p str)
        (let ((accum 0))
          (dotimes (pos (length str))
            (setf accum (+ (* accum 10)
                           (digit-char-p (char str pos)))))
          accum)
      nil))

這個定義演示了在 Common Lisp 中，字元是如何轉成數字的 – 函數 ``digit-char-p`` 不僅測試一個字元是否為數字，也回傳了對應的整數。

4.6 結構 (Structures)
===========================

結構可以想成是豪華版的向量。假設你要寫一個程式來追蹤很多長方體。你可能會想用三個向量元素來表示長方體：高度、寬度及深度。你的程式會變得更容易讀，如果你與其使用原本的 ``svrefs`` ，而定義一個像是這樣

::

  (defun block-height (b) (svref b 0))

等等的函數來取代。你可以把結構想成是，這些函數都替你定義好了的向量。

要定義一個結構，我們使用 ``defstruct`` 。在最簡單的情況下，我們只要給出結構及欄位的名字就可以了：

::

  (defstruct point
    x
    y)

這定義了一個 ``point`` 具有兩個欄位 x 與 y 。它也隱性地定義了 ``make-point`` , ``point-p`` , ``copy-point`` , ``point-x`` 及 ``point-y`` 函數。

2.3 節提到 Lisp 程式可以寫 Lisp 程式。這是我們目前所看過的明顯例子之一。當你呼叫 ``defstruct`` 時，它自動寫好了其它幾個函數的定義。有了巨集，你將能夠自己來辦到同樣的事情（如果你需要的話，你甚至可以自己寫 ``defstruct`` ）。

每一個 ``make-point`` 的呼叫，會回傳一個新的 ``point`` 。我們可以藉由給予對應的關鍵字參數，來指定單一欄位的值：

::

  (setf p (make-point :x 0 :y 0))
  #S(POINT X 0 Y 0)

存取 ``point`` 欄位的函數不僅被定義成可取出數值，也可以與 ``setf`` 合作使用。

::

  > (point-x p)
  0
  > (setf (point-y p) 2)
  2
  > p
  #S(POINT X 0 Y 2)

定義一個結構也定義了一個以此為名的型別。每個點會是型別 ``point`` ，然後是 ``structure`` ，接著是 ``atom`` ，最後是 ``t`` 。所以使用 ``point-p`` 來測試某個東西是不是一個點，也可以使用通用性的函數，像是 ``typep`` 來測試。

我們可以藉由在本來的定義中，附上一個含有欄位名及一個預設表達式的列表，來指定結構欄位的預設值。

::

  (defstruct polemic
    (type (progn
            (format t "What kind of polemic was it? ")
            (read)))
    (effect nil))

如果 ``make-polemic`` 呼叫沒有替這些欄位指定初始值，他們會被設成對應表達式的值：

::

  > (make-polemic)
  What kind of polemic was it? scathing
  #S(POLEMIC TYPE SCATHING EFFECT NIL)

我們也可以控制結構顯示的方式，以及結構產生的存取函數的字首。這裡是一個更詳細的做了這兩件事的 ``point`` 定義：

::

  (defstruct (point (:conc-name p)
                    (:print-function print-point))
    (x 0)
    (y 0))

  (defun print-point (p stream depth)
    (format stream "#<~A, ~A>" (px p) (py p)))

``:conc-name`` 參數指定了要放在欄位名前面的名字，並用這些名字來生成存取函數。預設是 ``point-`` ；現在變成只有 ``p`` 。不使用預設的方式，使你的程式碼的可讀性降低了一點，所以你只有在會常常用到這些存取函數時，你才會想做這類的事情。

``:print-function`` 是在它需要被顯示時，應該要用的函數\ *名* – 比如，頂層要顯示時。這個函數需要接受三個參數：要被印出的結構，在哪裡被印出，第三個參數通常可以被忽略。 [2]_ 我們會在 7.1 節討論這些流 (stream)。對現在來說，只要知道做為參數的流可以傳給 ``format`` 就好了。

函數 ``print-point`` 會用縮寫的形式來顯示點：

::

  > (make-point)
  #<0,0>

4.7 範例：二元搜索樹 (Example: Binary Search Tree)
======================================================

因為 ``sort`` 本身就內建了，你會很少，如果有的話，需要在 Common Lisp 裡寫排序程序。本節演示如何解決一個相關的問題，這個問題尚未有現成的解決方案：維護一個已排序的物件集合。本節的程式碼會把物件存在二元搜索樹裡（ *binary search tree* ）或稱作 BST。當二元搜索樹平衡時，它允許我們可以在與時間成 ``log n`` 比例的時間內，來尋找、新增或是刪除元素，其中 n 是集合的大小。

.. figure:: ../images/Figure-4.4.png

圖 4.4: 二元搜索樹

一個二元搜索樹是一種二元樹，其中給定某個排序函數 ``<`` ，每個元素的左子樹都 ``<`` 該元素，而該元素 ``<`` 其右子樹。圖 4.4 展示一個根據 ``<`` 排序的範例。

圖 4.5 包含了二元搜索樹中，插入與尋找的函數。基本的資料結構會是 ``node`` （節點），它有三個欄位：一個是存在該節點的物件，以及各一個欄位，給節點的左子樹及右子樹。你可以把節點想成是有一個 ``car`` 和兩個 ``cdr`` 的一個 cons 核（cons cell）。

::

  (defstruct (node (:print-function
                    (lambda (n s d)
                      (format s "#<~A>" (node-elt n)))))
    elt (l nil) (r nil))

  (defun bst-insert (obj bst <)
    (if (null bst)
        (make-node :elt obj)
        (let ((elt (node-elt bst)))
          (if (eql obj elt)
              bst
              (if (funcall < obj elt)
                  (make-node
                     :elt elt
                     :l (bst-insert obj (node-l bst) <)
                     :r (node-r bst))
                  (make-node
                     :elt elt
                     :r (bst-insert obj (node-r bst) <)
                     :l (node-l bst)))))))

  (defun bst-find (obj bst <)
    (if (null bst)
        nil
        (let ((elt (node-elt bst)))
          (if (eql obj elt)
              bst
              (if (funcall < obj elt)
                  (bst-find obj (node-l bst) <)
                  (bst-find obj (node-r bst) <))))))

  (defun bst-min (bst)
    (and bst
         (or (bst-min (node-l bst)) bst)))

  (defun bst-max (bst)
    (and bst
         (or (bst-max (node-r bst)) bst)))

圖 4.5 二元搜索樹：查詢與插入

一個二元搜索樹可以是 ``nil`` 或是一個左子、右子樹都是二元搜索樹的節點。如同列表可由連續呼叫 ``cons`` 來創建，二元搜索樹將可以由連續呼叫 ``bst-insert`` 來創建。這個函數接受一個物件，一個二元搜索樹及一個排序函數，並回傳一個包含此物件的二元搜索樹。和 ``cons`` 函數一樣， ``bst-insert`` 不改動做為第二個參數傳入的二元搜索樹。以下是我們如何使用它，來創建一個二元搜索樹：

::

  > (setf nums nil)
  NIL
  > (dolist (x '(5 8 4 2 1 9 6 7 3))
      (setf nums (bst-insert x nums #'<)))
  NIL

圖 4.4 顯示了此時 ``nums`` 的結構所對應的樹。

我們可以使用 ``bst-find`` ，它與 ``bst-insert`` 接受同樣的參數，來找到二元搜索樹中的物件。 先前敘述所提到的 ``node`` 結構，它像是一個具有兩個 ``cdr`` 的 cons 核。如果我們把 16 頁的 ``our-member`` 拿來與 ``bst-find`` 比較的話，這樣的類比變得更清楚。

和 ``member`` 一樣， ``bst-find`` 不僅回傳要尋找的元素，也回傳了被找元素做為根節點的子樹：

::

  > (bst-find 12 nums #'<)
  NIL
  > (bst-find 4 nums #'<)
  #<4>

這讓我們可以區分出無法找到某物以及成功找到 ``nil`` 的情況。

要找到二元搜索樹的最小及最大的元素是很簡單的。要找到最小的，我們隨著左子的路徑走，如同 ``bst-min`` 所做的。要找到最大的，我們隨著右子的路徑走，如同 ``bst-max`` 所做的：

::

  > (bst-min nums)
  #<1>
  > (bst-max nums)
  #<12>

要從二元搜索樹移除一個元素一樣很快，但需要更多程式碼。圖 4.6 演示了如何做到這件事。

::

  (defun bst-remove (obj bst <)
    (if (null bst)
        nil
        (let ((elt (node-elt bst)))
          (if (eql obj elt)
              (percolate bst)
              (if (funcall < obj elt)
                  (make-node
                     :elt elt
                     :l (bst-remove obj (node-l bst) <)
                     :r (node-r bst))
                  (make-node
                     :elt elt
                     :r (bst-remove obj (node-r bst) <)
                     :l (node-l bst)))))))

  (defun percolate (bst)
    (cond ((null (node-l bst))
           (if (null (node-r bst))
               nil
               (rperc bst)))
          ((null (node-r bst)) (lperc bst))
          (t (if (zerop (random 2))
                 (lperc bst)
                 (rperc bst)))))

  (defun rperc (bst)
    (make-node :elt (node-elt (node-r bst))
               :l (node-l bst)
               :r (percolate (node-r bst))))

圖 4.6 二元搜索樹：刪除

**勘誤:** 此版 ``bst-remove`` 定義已被回報是壞掉的，請參考 `這裡 <https://gist.github.com/2868263>`_ 獲得一個修補後的版本。

函數 ``bst-remove`` 接受一個物件，一個二元搜索樹以及一個排序函數，並回傳一個像是本來的二元搜索樹，但不含那個要移除的物件。跟 ``remove`` 一樣，它不改動做為第二個參數傳入的二分搜索樹：

::

  > (setf nums (bst-remove 2 nums #'<))
  #<5>
  > (bst-find 2 nums #'<)
  NIL

此時 ``nums`` 應有像是圖 4.7 所顯示的結構。（另一個可能性是 1 取代了 2 的位置。）

.. figure:: ../images/Figure-4.7.png

圖 4.7: 二元搜索樹

刪除需要更多工作因為從內部節點移除一個物件，會留下一個空缺，需要由其中一個孩子來填補。這是 ``percolate`` 函數的用途。它替換一個二元搜索樹的樹根（topmost element）時，用其中一個孩子來替換，並用此孩子的孩子來填補，如此這般一直做下去。

為了要保持樹的平衡，如果有兩個孩子時， ``perlocate`` 隨機擇一替換。表達式 ``(random 2)`` 會回傳 0 或 1，所以 ``(zerop (random 2))`` 會回傳真或假。

::

  (defun bst-traverse (fn bst)
    (when bst
      (bst-traverse fn (node-l bst))
      (funcall fn (node-elt bst))
      (bst-traverse fn (node-r bst))))

圖 4.8 二元搜索樹：走訪

一旦我們把一個物件集合插入至二元搜索樹時，中序走訪會將它們由小至大排序。這是圖 4.8 中， ``bst-traverse`` 函數的用途：

::

  > (bst-traverse #'princ nums)
  13456789
  NIL

（函數 ``princ`` 僅顯示一個單一物件）

本節所給出的程式碼，提供了一個二元搜索樹實作的骨架。你可能想根據應用需求，來充實其骨架。舉例來說，這裡所給出的程式碼每個節點只有一個 ``elt`` 欄位；在許多應用裡，有兩個欄位會更有意義， ``key`` 與 ``value`` 。本章的這個版本把二元搜索樹視為集合看待，從這個角度看，重複的插入是被忽略的。但是程式碼可以很簡單地改動，來處理重複的元素。

二元搜索樹不僅是維護一個已排序物件的集合的方法。他們是否是最好的方法，取決於你的應用。一般來說，二元搜索樹最適合用在插入與刪除是均勻分布的情況。有一件他們不適合的事，是用來維護優先佇列（priority queues）。在一個優先佇列裡，插入也許是均勻分布的，但刪除總是在一個末端。這會導致一個二元搜索樹變得不平衡，而我們所期望的複雜度是 ``O(log(n))`` 插入與刪除操作，會變成 ``O(n)`` 。如果你用二元搜索樹來表示一個優先佇列，你也可以使用一般的列表，因為二元搜索樹最終會作用的像個列表。

4.8 雜湊表 (Hash Table)
=====================================

第三章演示了列表可以用來表示集合（sets）與映射（mappings）。當列表的長度大幅上升時（或是 10 個元素），使用雜湊表會來得比較快。你透過呼叫 ``make-hash-table`` 來創建一個雜湊表，它不需要傳入參數：

::

  > (setf ht (make-hash-table))
  #<Hash-Table BF0A96>

和函數一樣，雜湊表總是用 ``#<...>`` 的形式來顯示。

一個雜湊表，像是一個關聯列表，是一種表達相關物件的方式。要取出與一給定鍵值有關的數值，我們呼叫 ``gethash`` 並傳入一個鍵值與雜湊表 。預設情況下，如果沒有與這個鍵值相關的數值， ``gethash`` 會回傳 ``nil`` 。

::

  > (gethash 'color ht)
  NIL
  NIL

在這裡我們首次看到 Common Lisp 最突出的特色之一：一個表達式可以回傳多個數值。函數 ``gethash`` 回傳兩個數值。第一個值是與鍵值有關的數值，第二個值說明了雜湊表是否有任何用此鍵值來儲存的數值。因為第二個值是 ``nil`` ，我們知道第一個 ``nil`` 是預設的回傳值，而不是因為 ``nil`` 是與 ``color`` 有關的數值。

大部分的實作會在頂層顯示一個函數呼叫的所有回傳值，但僅期待一個回傳值的程式碼，會只收到第一個回傳值。 5.5 節會說明程式碼是如何接收多個回傳值。

要把一個數值與鍵值作關聯，我們使用 ``gethash`` 搭配 ``setf`` ：

::

  > (setf (gethash 'color ht) 'red)
  RED

現在如果我們再次呼叫 ``gethash`` ，我們會得到我們剛插入的值：

::

  > (gethash 'color ht)
  RED
  T

第二個回傳值證明，我們取得了一個真正儲存的物件，而不是預設值。

存在雜湊表的物件或是鍵值可以是任何型別。舉例來說，如果我們要保留函數的某種訊息，我們可以使用一個雜湊表，用函數做為鍵值，字串做為詞條（entry）：

::

  > (setf bugs (make-hash-table))
  #<Hash-Table BF4C36>
  > (push "Doesn't take keyword arguments."
          (gethash #'our-member bugs))
  ("Doesn't take keyword arguments.")

由於 ``gethash`` 預設回傳 ``nil`` ，而 ``push`` 是 ``setf`` 的縮寫，我們可以簡單地把新的字串推入一個函數的詞條。（有困擾的 ``our-member`` 定義在 16 頁。）

你可以用雜湊表取代列表來表示集合。當集合變大時，雜湊表的查詢與刪除應該比較快。要新增一個成員到用雜湊表所表示的集合，把 ``gethash`` 用 ``setf`` 設成 ``t`` ：

::

  > (setf fruit (make-hash-table))
  #<Hash-Table BFDE76>
  > (setf (gethash 'apricot fruit) t)
  T

然後要測試是否為成員，你只要呼叫：

::

  > (gethash 'apricot fruit)
  T
  T

由於 ``gethash`` 預設回傳真，一個新創的雜湊表，很方便地是一個空集合。

要從集合中移除一個物件，你可以呼叫 ``remhash`` ，它從一個雜湊表中移除一個詞條（entry）：

::

  > (remhash 'apricot fruit)
  T

回傳值說明了那裡是否有詞條被移除；在這個情況裡，有。

雜湊表有一個迭代函數： ``maphash`` ，它接受一個兩個參數的函數及一個雜湊表。函數會被每個鍵值對呼叫，沒有特定的順序：

::

  > (setf (gethash 'shape ht) 'spherical
          (gethash 'size ht) 'giant)
  GIANT

  > (maphash #'(lambda (k v)
                 (format t "~A = ~A~%" k v))
             ht)
  SHAPE = SPHERICAL
  SIZE = GIANT
  COLOR = RED
  NIL

它總是回傳 ``nil`` ，但你可以透過傳入一個會累積數值的函數，把它們存在一個列表裡。

雜湊表可以容納任何數目的元素，因為當空間用完時，它們會被擴張。如果你想要確保一個雜湊表，從特定數目的元素空間開始時，你可以給一個選擇性的 ``:size`` 參數給 ``make-hash-table`` 。做這件事情有兩個理由：因為你知道雜湊表會變得很大，你想要避免擴張它；或是因為你知道雜湊表會是很小，你不想要浪費記憶體。 ``:size`` 參數不僅指定了雜湊表的空間，也指定了元素的數量。平均來說，在被擴張前所能夠容納的數量。所以

``(make-hash-table :size 5)``

會回傳一個預期存放五個元素的雜湊表。

和任何牽涉到查詢的結構一樣，雜湊表一定有某種比較鍵值的概念。預設是使用 ``eql`` ，但你可以提供一個額外的參數 ``:test`` 來告訴一個雜湊表要使用 ``eq`` ， ``equal`` ，還是 ``equalp`` ：

::

  > (setf writers (make-hash-table :test #'equal))
  #<Hash-Table C005E6>
  > (setf (gethash '(ralph waldo emerson) writers) t)
  T

這是一個我們要使雜湊表有效率的取捨之一。有了列表，我們可以指定 ``member`` 來判斷相等的判斷式。有了雜湊表，我們可以預先決定，並在雜湊表創建時指定它。

大多數 Lisp 程式設計的的取捨（或是生活，就此而論）都有這種特質。起初你想要事情進行得流暢，甚至賠上效率的代價。之後，當程式碼變得沈重時，你犧牲了彈性來換取速度。

Chapter 4 總結 (Summary)
============================

1. Common Lisp 支援至少 7 個維度的陣列。一維陣列稱為向量。
2. 字串是字元的向量。字元本身就是物件。
3. 序列包括了向量與列表。許多序列函數都接受標準的關鍵字參數。
4. 因為有許多函數都支援字串，所以在 Lisp 裡做解析是容易的。
5. 呼叫 ``defstruct`` 定義了一個帶有命名欄位的結構。它是一個程式能寫出程式的好例子。
6. 二元搜索樹見長於維護一個已排序的物件集合。
7. 雜湊表提供了一個更有效率的方式來表示集合（sets）與映射 (mappings)。

Chapter 4 練習 (Exercises)
==================================

1. 定義一個函數，接受一個平方陣列（square array, 一個相同維度的陣列 ``(n n)`` )，並將它順時針轉 90 度。

::

  > (quarter-turn #2A((a b) (c d)))
  #2A((C A) (D B))

你會需要用到 361 頁的 ``array-dimensions`` 。

2. 閱讀 368 頁的 ``reduce`` 說明，然後用它來定義：

::

  (a) copy-list
  (b) reverse（針對列表）

3. 定義一個結構來表示一個樹，其中每個節點包含某些資料及三個小孩。定義：

::

  (a) 一個函數來複製這樣的樹（複製完的節點與本來的節點是不相等(eql)的）
  (b) 一個函數，接受一個物件與這樣的樹，如果物件與樹中各節點的其中一個欄位相等時，回傳真。

4. 定義一個函數，接受一個二元搜索樹，並回傳由此樹元素所組成的，一個由大至小排序的列表。

5. 定義 ``bst-adjoin`` 。這個函數應與 ``bst-insert`` 接受相同的參數，但應該只在物件不等於任何樹中物件時將其插入。

**勘誤:** ``bst-adjoin`` 的功能與 ``bst-insert`` 一模一樣。

6. 任何雜湊表的內容可以由關聯列表(assoc-list)來描述，其中列表的元素是 ``(k . v)`` 的形式，對應到雜湊表中的每一個鍵值對。定義一個函數：

::

  (a) 接受一個關聯列表，並回傳一個對應的雜湊表。
  (b) 接受一個雜湊表，並回傳一個對應的關聯列表。

.. rubric:: 腳註

.. [1] 一個簡單的陣列是不可調整的(neither adjustable)、不可替換的 (nor displaced)，且沒有填充指標 (fill-pointer)。陣列預設是簡單的。一個簡單向量是一個一維簡單陣列，可以含有任何型別的元素。

.. [2] 在 Ansi Common Lisp 裡，你可以給一個 ``:print-object`` 的關鍵字參數來取代，它只需要兩個參數。也有一個巨集叫做 ``print-unreadable-object`` ，在可用時，應該要使用這個，可以用 ``#<...>`` 的語法來顯示物件。
