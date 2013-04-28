.. highlight:: cl
   :linenothreshold: 0

备注
******************************

本节既是备注亦作为参考文献。所有列于此的书籍与论文皆值得阅读。

**译注: 备注后面跟随的数字即书中的页码**

备注 viii (Notes viii)
==================================

`Steele, Guy L., Jr. <http://en.wikipedia.org/wiki/Guy_L._Steele,_Jr.>`_\ , `Scott E. Fahlman <http://en.wikipedia.org/wiki/Scott_Fahlman>`_\ , `Richard P. Gabriel <http://en.wikipedia.org/wiki/Richard_P._Gabriel>`_\ , `David A. Moon <http://en.wikipedia.org/wiki/David_Moon>`_\ , `Daniel L. Weinreb <http://en.wikipedia.org/wiki/Daniel_Weinreb>`_ , `Daniel G. Bobrow <http://en.wikipedia.org/wiki/Daniel_G._Bobrow>`_\ , `Linda G. DeMichiel <http://www.informatik.uni-trier.de/~ley/db/indices/a-tree/d/DeMichiel:Linda_G=.html>`_\ , `Sonya E. Keene <http://www.amazon.com/Sonya-E.-Keene/e/B001ITVL6O>`_\ , `Gregor Kiczales <http://en.wikipedia.org/wiki/Gregor_Kiczales>`_\ , `Crispin Perdue <http://perdues.com/CrisPerdueResume.html>`_\ , `Kent M. Pitman <http://en.wikipedia.org/wiki/Kent_Pitman>`_\ , `Richard C. Waters <http://www.rcwaters.org/>`_\ , 以及 John L White。 `Common Lisp: the Language, 2nd Edition. <http://www.cs.cmu.edu/Groups/AI/html/cltl/cltl2.html>`_ Digital Press, Bedford (MA), 1990.

备注 1 (Notes 1)
==================================

`McCarthy, John. <http://en.wikipedia.org/wiki/John_McCarthy_(computer_scientist)>`_ `Recursive Functions of Symbolic Expressions and their Computation by Machine, Part I. <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.91.4527&rep=rep1&type=pdf>`_ CACM, 3:4 (April 1960), pp. 184-195.

`McCarthy, John. <http://en.wikipedia.org/wiki/John_McCarthy_(computer_scientist)>`_ `History of Lisp. <http://www-formal.stanford.edu/jmc/history/lisp/lisp.html>`_ In `Wexelblat, Richard L. <http://en.wikipedia.org/wiki/Richard_Wexelblat>`_ (Ed.) `Histroy of Programming Languages. <http://cs305.com/book/programming_languages/Conf-01/HOPLII/frontmatter.pdf>`_ Academic Press, New York, 1981, pp. 173-197.

备注 3 (Notes 3)
==================================

备注 4 (Notes 4)
==================================

备注 5 (Notes 5)
==================================

备注 5-2 (Notes 5-2)
==================================

备注 12 (Notes 12)
==================================

备注 17 (Notes 17)
==================================

备注 26 (Notes 26)
==================================

备注 28 (Notes 28)
==================================

备注 46 (Notes 46)
==================================

备注 61 (Notes 61)
==================================

备注 62 (Notes 62)
==================================

备注 76 (Notes 76)
==================================

备注 81 (Notes 81)
==================================

备注 84 (Notes 84)
==================================

备注 89 (Notes 89)
==================================

备注 91 (Notes 91)
==================================

备注 94 (Notes 94)
==================================

备注 95 (Notes 95)
==================================

备注 100 (Notes 100)
==================================

备注 100-2 (Notes 100-2)
==================================

备注 106 (Notes 106)
==================================

备注 109 (Notes 109)
==================================

备注 109-2 (Notes 109-2)
==================================

备注 112 (Notes 112)
==================================

备注 123 (Notes 123)
==================================

备注 125 (Notes 125)
==================================

备注 141 (Notes 141)
==================================

备注 141-2 (Notes 141-2)
==================================

备注 150 (Notes 150)
==================================

下面这个函数会显示在一个给定实现中，16 个用来标示浮点表示法的限制的全局常量：

::

	(defun float-limits ()
	  (dolist (m '(most least))
	    (dolist (s '(positive negative))
	      (dolist (f '(short single double long))
	        (let ((n (intern (string-upcase
	                            (format nil "~A-~A-~A-float"
	                                          m  s  f)))))
	          (format t "~30A ~A ~%" n (symbol-value n)))))))

备注 164 (Notes 164)
==================================

`快速排序演算法 <http://zh.wikipedia.org/zh-cn/%E5%BF%AB%E9%80%9F%E6%8E%92%E5%BA%8F>`_\ 由\ `霍尔 <http://zh.wikipedia.org/zh-cn/%E6%9D%B1%E5%B0%BC%C2%B7%E9%9C%8D%E7%88%BE>`_\ 于 1962 年发表，并被描述在 Knuth, D. E. *Sorting and Searching.* Addison-Wesley, Reading (MA), 1973.一书中。

备注 173 (Notes 173)
==================================

`Foderaro, John K.  Introduction to the Special Lisp Section. CACM 34:9 (Setember 1991), p.27 <http://www.informatik.uni-trier.de/~ley/db/journals/cacm/cacm34.html>`_

备注 176 (Notes 176)
===============================

关于 CLOS 更详细的信息，参考下列书目：

Keene, Sonya E. `Object Oriented Programming in Common Lisp <http://en.wikipedia.org/wiki/Object-Oriented_Programming_in_Common_Lisp:_A_Programmer's_Guide_to_CLOS>`_ , Addison-Wesley, Reading (MA), 1989

Kiczales, Gregor, Jim des Rivieres, and Daniel G. Bobrow. `The Art of the Metaobject Protocol <http://en.wikipedia.org/wiki/The_Art_of_the_Metaobject_Protocol>`_ MIT Press, Cambridge, 1991

备注 178 (Notes 178)
==============================

让我们再回放刚刚的句子一次：\ *我们甚至不需要看程序中其他的代码一眼，就可以完成种种的改动。*\ 这个想法或许对某些读者听起来担忧地熟悉。这是写出\ `面条式代码 <http://zh.wikipedia.org/wiki/%E9%9D%A2%E6%9D%A1%E5%BC%8F%E4%BB%A3%E7%A0%81>`_\ 的食谱。

面向对象模型使得通过一点一点的来构造程序变得简单。但这通常意味著，在实践上它提供了一种有结构的方法来写出面条式代码。这不一定是坏事，但也不会是好事。

很多现实世界中的代码是面条式代码，这也许不能很快改变。针对那些终将成为面条式代码的程序来说，面向对象模型是好的：它们最起码会是有结构的面条。但针对那些也许可以避免误入崎途的程序来说，面向对象抽象只是更加危险的，而不是有用的。

备注 178 (Notes 178)
==================================

备注 183 (Notes 183)
==================================

备注 191 (Notes 191)
==================================

备注 204 (Notes 204)
==================================

备注 213 (Notes 213)
==================================

Knuth, Donald E. `Structured Programming with goto Statements. <http://sbel.wisc.edu/Courses/ME964/Literature/knuthProgramming1974.pdf>`_ *Computing Surveys* , 6:4 (December 1974), pp. 261-301

备注 214 (Notes 214)
==================================

Knuth, Donald E. `Computer Programming as an Art <http://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=2&cad=rja&ved=0CC4QFjAB&url=http%3A%2F%2Fawards.acm.org%2Fimages%2Fawards%2F140%2Farticles%2F7143252.pdf&ei=vl9VUIWBIOWAmQWQu4FY&usg=AFQjCNHAgYS4PiHA0OfgOdiDfPU2i6HAmw&sig2=zZalr-ife4DB4BR2CPORBQ>`_ *In ACM Turing Award Lectures: The First Twenty Years.* ACM Press, 1987

备注 216 (Notes 216)
==================================

备注 217 (Notes 217)
==================================

备注 218 (Notes 218)
==================================

备注 219 (Notes 219)
==================================

备注 224 (Notes 224)
==================================

备注 229 (Notes 229)
==================================

备注 230 (Notes 230)
==================================

备注 239 (Notes 239)
==================================

备注 242 (Notes 242)
==================================

备注 248 (Notes 248)
==================================

关于更深入讲述逻辑推论的资料，参见：\ `Stuart Russell <http://www.cs.berkeley.edu/~russell/>`_ 及 `Peter Norvig <http://www.norvig.com/>`_ 所著的 `Artificial Intelligence: A Modern Approach <http://aima.cs.berkeley.edu/>`_\ 。

备注 276 (Notes 276)
==================================

备注 284 (Notes 284)
==================================

备注 284-2 (Notes 284-2)
==================================

Gabriel, Richard P. `Lisp Good News, Bad News, How to Win Big <http://www.dreamsongs.com/Files/LispGoodNewsBadNews.pdf>`_ *AI Expert*\ , June 1991, p.35.

早在 1973 年，`Richard Fateman <http://en.wikipedia.org/wiki/Richard_Fateman>`_ 已经能证明在 `PDP-10 <http://en.wikipedia.org/wiki/PDP-10>`_ 主机上，`MacLisp <http://en.wikipedia.org/wiki/Maclisp>`_ 编译器比制造商的 FORTRAN 编译器，产生出更快速的代码。

**译注:** `该篇 MacLisp 编译器在 PDP-10 可产生比 Fortran 快的代码的论文在这可以找到 <http://dl.acm.org/citation.cfm?doid=1086803.1086804>`_

备注 399 (Notes 399)
==================================

