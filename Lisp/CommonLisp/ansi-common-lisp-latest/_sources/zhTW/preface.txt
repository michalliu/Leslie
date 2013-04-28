前言
***************

本書目的是快速而全面的教你 Common Lisp 的有關知識。它實際上包含兩本書。前半部分用大量的例子來解釋 Common Lisp 裡面重要的概念。後半部分是一個最新 Common Lisp 辭典，它裡面包括了所有 ANSI Common Lisp 的運算元。

本書目標讀者
====================

ANSI Common Lisp 這本書適合學生或者是專業的程式設計師去讀。本書假設讀者在讀它之前沒有有關 Lisp 的知識。有別的程式語言的程式設計經驗也許對讀本書有益處，但也不是必須的。本書從解釋 Lisp 中最基本的概念開始，並且對於初學 Lisp 的人們最容易迷惑的地方進行特別的強調。

本書也可以作為教授 Lisp 程式設計的課本，也可以作為人工智慧課程和其他程式語言課程中有關 Lisp 部分的參考書。想要學習 Lisp 的專業程式設計師肯定會很喜歡貫穿於本書中的著眼於實踐的理念。那些已經在使用 Lisp 寫程式的人士，也會發現，本書裡面有許多很好的實例可供參考，而本書也是一本很方便的 ANSI Common Lisp 參考書。

如何使用這本書
====================

學習 Lisp 最好的辦法就是拿它來寫程式。況且，在學習的同時用你學到的技術設計程式也是非常有趣的一件事。本書的編寫目的就是讓讀者盡快的入門，在對 Lisp 進行簡短的介紹之後，
第 2 章開始用 21 頁的容量介紹了著手編寫 Lisp 程式時可能用到所有知識。
3-9 章講解了 Lisp 裡面一些重要的觀念。這些章節特別強調了一些重要的概念比如 Lisp 裡面指標的角色，如何使用遞迴來解決問題，以及第一級函數 (first-class function)的重要性。

針對那些想要更深入了解 Lisp 的讀者：
10-14 章包含了巨集 (macro)，CLOS (Common Lisp Object System)，列表操作 (list operation)，程式最佳化 (optimization)，以及一些更高階的議題比如套件 (package)和讀取巨集 (read-macro)。

15-17 章用 3 個 Common Lisp 的實際應用，總結了之前章節中所講解的知識：一個是進行邏輯推理的程式，另外一個是一個 HTML 生成器，最後一個是針對物件導向程式設計的嵌入式語言。

本書的最後一部分包含 4 個附錄，這些附錄應該對所有的讀者都有用：
附錄 A-D 包括了一個如何調試程式的指南， 58 個 Common Lisp 運算元的原始碼，一個對 ANSI Common Lisp 和之前的 Lisp 語言的區別的總結，以及一個包括所有 ANSI Common Lisp 的參考手冊。

本書還包括一部分註釋。這些註釋包括一些說明，一些參考條目，一些額外的代碼，以及一些會偶然出現的對不正確表述的糾正。註釋在文中用一個小圓圈來表示，像這樣：○

程式碼
==========

雖然本書介紹的是 ANSI Common Lisp ，但是本書中的程式碼可以在任何版本的 Common Lisp 中運行。那些依賴 Lisp 語言新特性的例子的旁邊，會有註釋告訴你如何把它們運行於舊版本的 Lisp 中。

本書中所有的程式碼都可以在網路上下載到。你可以在網路上找到這些程式碼，它們還附帶著一個免費軟體的連結，一些過去的論文，以及 Lisp 的 FAQ 。還有很多有關 Lisp 的資源可以在此找到： http://www.eecs.harvard.edu/onlisp/
源程式碼可以在此 FTP 服務器上下載：
ftp://ftp.eecs.harvard.edu:/pub/onlisp/
讀者的問題和意見可以發送到 pg@eecs.harvard.edu 。

譯註：下載的連結已失效，請參考這裡：http://lib.store.yahoo.net/lib/paulgraham/acl2.lisp

On Lisp
==========

在整本 On Lisp 書中，我一直試著指出一些 Lisp 獨一無二的特性，這些特性使得 Lisp 更像 “Lisp” 。並且我將展示一些 Lisp 能讓你完成的新事情。比如說宏： Lisp 程式設計師能夠並且經常編寫一些能夠寫程式的程式。對於程式生成程式這種特性，因為 Lisp 是主流語言中唯一一個提供一些方便的抽象讓你完成這個任務的程式語言，所以 Lisp 是主流語言中唯一一個廣泛運用這個特性的語言。我非常樂意邀請那些想要更進一步了解巨集和其他高級 Lisp 技術的讀者讀一下本書的姐妹篇： On Lisp 。

誌謝
==========

在所有幫助我完成這本的朋友當中，我想特別的感謝一下 Robert Morris 。他的重要影響反應在整本書中。他的這樣影響讓這本書更加優秀。本書中好一些實際程式都源自他手。這些程式包括 138 頁的 Henley 和 249 頁的模式匹配器。

我非常的高興我有一個高水平的技術審稿小組： Skona Brittain, John Foderaro, Nick Levine, Peter Norvig 和 Dave Touretzky 。本書中幾乎所有部分都得益於它們的意見。 John Foderaro 甚至重寫了本書 5.7 節中一些程式碼。

另外一些人通篇閱讀了本書的手稿，它們是： Ken Anderson, Tom Cheatham, Richard Fateman, Steve Hain, Barry Margolin, Waldo Pacheco, Wheeler Ruml 和 Stuart Russell。特別提到的是，Ken Anderson 和 Wheeler Ruml 給予了很多很多有幫助的意見。

我非常感謝 Cheatham 教授，更廣泛的說，哈佛，給我提供了編寫這本書的一些必要的設施。另外也要感謝 Aiken 實驗室的人員： Tony Hartman, Dave Mazieres, Janusz Juda, Harry Bochner 和 Joanne Klys。

我非常高興能再一次有機會和 Alan Apt 一起工作。這些在 Prentice Hall 工作的人士: Alan, Mona, Pompili Shirley McGuire 和 Shirley Michaels, 與你們一起工作我很高興。

本書用 Leslie Lamport 寫的 LaTeX 進行排版。 LaTeX 是在 Donald Knuth 編寫的 TeX 的基礎上，又加了 LACarr, Van Jacobson 和 Guy Steele 所編寫的巨集完成。書中的圖表是由 John Vlissides 和 Scott Stanton 編寫的 Idraw 完成的。整本書的預覽是由 Tim Theisen 寫的 Ghostview 完成的。 Ghostview 是根據 L. Peter Deutsch 的 Ghostscript 創建的。

我還需要感謝其他的許多人，包括：Henry Baker, Kim Barrett, Ingrid Bassett, Trevor Blackwell, Paul Becker, Gary Bisbee, Frank Deutschmann, Frances Dickey, Rich 和 Scott Draves, Bill Dubuque, Dan Friedman, Jenny Graham, Alice Hartley, David Hendler, Mike Hewett, Glenn Holloway, Brad Karp, Sonya Keene, Ross Knights, Mutsumi Komuro, Steffi Kutzia, David K​​uznick, Madi Lord, Julie Mallozzi, Paul McNamee, Dave Moon, Howard Mullings, Mark Nitzberg, Nancy Parmet 和其家人, Robert Penny, Mike Plusch, Cheryl Sacks, Hazem Sayed, Shannon Spires, Lou Steinberg, Paul Stoddard, John Stone, Guy Steele, Steve Strassmann, Jim Veitch, Dave Watkins, Idelle and Julian Weber, the Weickers, Dave Yost 和 Alan Yuille。

另外，著重感謝我的父母和 Jackie。

高德納給他的經典叢書起名叫《計算機程式設計藝術》。在他的圖靈獎獲獎感言中，他解釋說這本書的書名源於他內心深處潛意識––他的潛意識告訴他程式設計其實就是尋求撰寫最優美的程式。

就像建築設計一樣，程式設計即是一門工程技術又是一門藝術。一個程式也既要遵循數學原理又要符合物理定律。但是建築師的目的不僅僅是建一個不會倒塌的建築。更重要的是，他們要建一個優美的建築。

像高德納一樣，很多程式設計師認為程式設計的真正目的也不僅僅是寫出正確的程式，更重要是寫出優美的程式。幾乎所有的 Lisp 駭客也是這麼想的。 Lisp 駭客精神可以用兩句話來概括：程式設計應該是很有趣的。程式應該很優美。這就是我在這本書中想要傳達的精神。

保羅•葛拉漢姆 (Paul Graham)