<?xml version="1.0"?>




<st-source>

<time-stamp>From VisualWorks® Personal Use Edition, 8.3 of 28 july 2017 on 16 january 2022 at 16:50:02</time-stamp>

<component-created>

<name>Programming Languages</name> <type>package</type>

</component-created><!-- Package Programming Languages* -->







<component-property>

<name>Programming Languages</name> <type>package</type>

<property>comment</property> <value>'Programs for the Programming Languages course.'</value>

</component-property>




<class>

<name>Wielokat</name>

<environment>Smalltalk</environment>

<super>Core.Object</super>

<private>false</private>

<indexed-type>none</indexed-type>

<inst-vars>wierzcholki nazwa </inst-vars>

<class-inst-vars></class-inst-vars>

<imports></imports>

<category>JezykiProgramowania</category>

<attributes>

<package>Programming Languages</package>

</attributes>

</class>




<comment>

<class-id>Wielokat</class-id>

<body>Programs for the Programming Languages course.</body>

</comment>




<class>

<name>Kwadrat</name>

<environment>Smalltalk</environment>

<super>Wielokat</super>

<private>false</private>

<indexed-type>none</indexed-type>

<inst-vars></inst-vars>

<class-inst-vars></class-inst-vars>

<imports></imports>

<category>JezykiProgramowania</category>

<attributes>

<package>Programming Languages</package>

</attributes>

</class>




<class>

<name>Trojkat</name>

<environment>Smalltalk</environment>

<super>Wielokat</super>

<private>false</private>

<indexed-type>none</indexed-type>

<inst-vars></inst-vars>

<class-inst-vars></class-inst-vars>

<imports></imports>

<category></category>

<attributes>

<package>Programming Languages</package>

</attributes>

</class>




<methods>

<class-id>Wielokat</class-id> <category>accessing</category>




<body package="Programming Languages" selector="obrot">obrot	"obraca wielokat o 90 stopni wedlug pierwszego wierzcholka"		2 to: (wierzcholki capacity) do: [:i| 		wierzcholki at: i put: (0 - ((wierzcholki at: i) y))@((wierzcholki at: i ) x).	].</body>




<body package="Programming Languages" selector="nazwa">nazwa	"podaje nazwe wielokata"	^nazwa</body>




<body package="Programming Languages" selector="drukuj">drukuj	"drukuje informacje o wielokacie"		Transcript show: nazwa printString.	Transcript show: ''; cr.  	Transcript show: ' Area: '.	Transcript show: self pole printString.	Transcript show: ''; cr.  	1 to: (wierzcholki capacity) do: [:i|  	Transcript show: ' Nr: '.	Transcript show: (i printString).	Transcript show: ' x: '.	Transcript show: (wierzcholki at: i) x printString.	Transcript show: ' y: '.	Transcript show: (wierzcholki at: i) y printString.	Transcript show: ''; cr.  	]."Stampa""stampa le informazioni su un poligono"</body>




<body package="Programming Languages" selector="nazwa:">nazwa: nowa_nazwa	"ustawia nowa nazwe wielokata"	nazwa:=nowa_nazwa</body>

</methods>




<methods>

<class-id>Wielokat</class-id> <category>initialize-release</category>




<body package="Programming Languages" selector="initialize:name:">initialize: liczbaWierzcholkow name: nowaNazwa	"konstruktor obiektu - wielokata"	nazwa:=nowaNazwa.	wierzcholki:=Array new: liczbaWierzcholkow.	wierzcholki at: 1 put: 0@0.</body>

</methods>







<methods>

<class-id>Kwadrat</class-id> <category>actions</category>




<body package="Programming Languages" selector="pole">pole	"licz pole kwadratu"	^(wierzcholki at: 2) x squared + (wierzcholki at: 2) y squared</body>

</methods>




<methods>

<class-id>Kwadrat</class-id> <category>arithmetic</category>




<body package="Programming Languages" selector="+">+ figura	"dodaj 2 figury w sensie pola"	| p |	p:=self pole + figura pole.	^(Kwadrat new) initialize: p sqrt</body>

</methods>




<methods>

<class-id>Kwadrat</class-id> <category>initialize-release</category>




<body package="Programming Languages" selector="initialize:">initialize: bok	"tworz kwadrat o podanym boku"	super initialize: 4 name: 'Kwadrat'.	wierzcholki at: 2 put: bok@0.	wierzcholki at: 3 put: bok@bok.	wierzcholki at: 4 put: 0@bok.</body>

</methods>







<methods>

<class-id>Trojkat class</class-id> <category>instance creation</category>




<body package="Programming Languages" selector="new">new	"Answer a newly created and initialized instance."	^super new initialize</body>

</methods>







<methods>

<class-id>Trojkat</class-id> <category>actions</category>




<body package="Programming Languages" selector="pole">pole 	"liczy pole szesciokata"		| b1 b2 c1 c2 |	b1 := ((wierzcholki at: 2) x).	b2 := ((wierzcholki at: 2) y).	c1 := ((wierzcholki at: 3) y) .	c2 :=  ((wierzcholki at: 3) x) abs.		b2 &lt; b1	ifTrue: [^((b1*c1)/2)] 	ifFalse: [^b2*c2/2].</body>

</methods>




<methods>

<class-id>Trojkat</class-id> <category>arithmetic</category>




<body package="Programming Languages" selector="+">+ figura	"dodaj 2 figury w sensie pola"	| newArea nowe base height b1 b2 h1 h2 k| 		b1 := ((wierzcholki at: 2) x).	b2 := ((wierzcholki at: 2) y).	h1 := ((wierzcholki at: 3) y) .	h2 :=  ((wierzcholki at: 3) x) abs.	b1 &gt; b2	ifTrue:[base:= b1. height:= h1.]	ifFalse:[base:= b2. height:= h2.].		newArea:=self pole + figura pole.	k:= ((newArea * 2) / (base * height)) sqrt.	nowe:=(Trojkat new) initialize: base*k  wysokosc: height*k.	^nowe"nowe=nuovo"</body>

</methods>




<methods>

<class-id>Trojkat</class-id> <category>initialize-release</category>




<body package="Programming Languages" selector="initialize:wysokosc:">initialize: base wysokosc: height	"tworz kwadrat o podanym boku"	super initialize: 3 name: 'Trojkat'.	wierzcholki at: 2 put: base@0.	wierzcholki at: 3 put: (base * 0.5)@height. "should be correct"</body>




<body package="Programming Languages" selector="initialize">initialize	"Initialize a newly created instance. This method must answer the receiver."	super initialize.	" *** Replace this comment with the appropriate initialization code *** "	^self</body>

</methods>










</st-source>
