---
layout: layout
title: Drakma a Quick Overview
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


