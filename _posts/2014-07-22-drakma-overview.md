---
layout: default
title: Drakma Quick Overview
---

### Overview:
I`ll be using this CL HTTP client called Drakma for future projets so I would like to go through some of its basic functionalities.

"Drakma is a full-featured HTTP client implemented in Common Lisp. It knows how to handle HTTP/1.1 chunking, persistent connections, re-usable sockets, SSL, continuable uploads, file uploads, cookies, and more."

Here is the developer`s [documentation](http://weitz.de/drakma/)

### Instalation:
Once you have quicklisp installed you could simply load it with:
{% highlight cl %}
(ql:quickload :drakma)
{% endhighlight %}

### Exhibit headers exchanged between Drakma and HTTP Request:
{% highlight cl %}
(setf drakma:*header-stream* *standard-output*)
{% endhighlight %}

### Example:
Since I\`m currently working on a project that involves OAI-PMH Protocol, I`ll build a simple lisp function that get some records in XML from [bibliotecadigital.fgv.br](http://sistema.bibliotecas.fgv.br/)
{% highlight cl %}
 (defun list-records (metadataPrefix)
	   (let ((req (concatenate 'string "http://bibliotecadigital.fgv.br/oai/request?verb=ListRecords&metadataPrefix=" metadataPrefix)))
	   (drakma:http-request req))) 
{% endhighlight %}

To make clear how to feed this function, we could just issue another OAI-PMH request that list the metada formats implemented by this repository.
To acomplish that first you`ll need to save a file which I called formats.xml in some folder:
{% highlight cl %}
(with-open-file (str "~/formats.xml" 
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create)
	   (format str (drakma:http-request "http://bibliotecadigital.fgv.br/oai/request?verb=ListMetadataFormats" :user-agent :mozilla)))
{% endhighlight %}
Realize that I faked the user agent and pretented to be using a mozilla browser, that is another cool feature of the drakma:http-request function.

So now I`ll use another package called cxml which will help you manage XML files, so let`s load it:
{% highlight cl %}
(ql:quickload :cxml)  
{% endhighlight %}
After that is done, we can parse the XML and save the parsed tree in a variable called *tree*:
{% highlight cl %}
(cxml:parse-file "~/formats.xml" (cxml-dom:make-dom-builder))
(defparameter *tree* *)
{% endhighlight %}
Now is where it gets interesting, we can finally use DOM to iterate in the tree to get the metadataPrefix implemented by this repository. To do that we may use the following piece of code:
{% highlight cl %}
(dom:do-node-list (n (dom:get-elements-by-tag-name (dom:document-element *tree*) "metadataPrefix")) (print(dom:node-value(dom:item(dom:child-nodes n)0))))
{% endhighlight %}
Let me brake this piece of code in bite sizes so it all get understandable. First I\`m doing the dom-version of the lisp function do-list which will iterate in the variable I called **n**(nodes). The list of nodes feed to do-list I got with the handy function **dom:get-elements-by-tag-name**. So I`m iterating only in the nodes **<metadataPrefix>**. Now comes the tricky part.


"In the DOM, everything is a node. Element nodes do not have a text value.

The text of an element node is stored in a child node. This node is called a text node.

The way to get the text of an element is to get the value of the child node (text node)."[DOM Reference](http://www.w3schools.com/dom/dom_nodes_get.asp)


So that is why i need to access the childs of n(**dom:child-nodes**), from those i want the first element(**dom:item**, similar to the CL **elt**) and then I can get the **node-value**.

Now I got the list:
"oai_dc" 
"rdf" 
"ore" 
"mets" 

So I can call the function previously defined as list-records to get the XML in one of these formats, such as:
{% highlight cl %}
(list-records "oai_dc")
{% endhighlight %}
 


