
git clone https://github.com/DamienCassou/pier-cl.git
cd pier-cl.git
./install.bash
... fait ton fichier .pier...
./pier.sh monfichier.pier
... et tu obtiens ton monfichier.pier.tex

What is missing
------------------------------------------
     defining a label for a reference
     defining a caption for a figure
     defining a figure
     todo
     comment
     having frame to produce frametext like notes or important @@important @@note

To embed a figure create a link of the form
+foo.png|width=100%+

To add a keyword to the index use +index:WAComponent+. To add a two level keyword use the syntax +index:WAComponent!renderContentOn:+. Note that this markup does not generate any output. Preferably put it at the beginning of the paragraph.

Source Code

For inlined source code use the syntax ==100 factorial== . Use 4 spaces to indent the code, no tabs.

=Integer>>factorial
=   ^ self < 2
=      ifTrue: [ 1 ]
=      ifFalse: [ self * (self - 1) factorial ]


To create references to chapters, sections or figures use the normal link-syntax but prepend it with ref:, for example:

*ref:foo.png*
*ref:../components*
This will create a link of the form Form 123 and Section 4.

Paragraph

Use the syntax @@important This is the paragraph text. to annotate a paragraph. Use annotations sparingly.

This is an @@important paragraph.

This is a @@blockquote paragraph.

This is a @@note paragraph.

This is an @@advanced paragraph.

This is a @@todo paragraph.




Paragraphs

A single newline has no effect on the layout. An empty line starts a new paragraph.
Sections

A line starting with ! becomes a section heading. Use multiple ! to create a subsection.
Horizontal Line

A line starting with _ (underline) becomes a horizontal line. This is often used to separate topics.
Lists

Lines starting with #, -, ; or : create a list:
A block of lines, where each line starts with - is transformed to a bulleted list, where each line is an entry.
A block of lines, where each line starts with # is transformed to an ordered list, where each line is an entry.
A block of lines, where each line starts with ; or : is transformed to a definition list, where the keyword is prepended with ; and the definition with :. Keywords and definitions can be mixed arbitrary.
Lists can also be nested. Thus, a line starting with #- is an element of a bulleted list that is part of an ordered list.
Tables

To create a table, start off the lines with | and separate the elements with |s. Each new line represents a new row of the table. The contents of cells can be aligned left, centered or aligned right by using |{, || or |} respectively. Add a single ! to let the cell become a table heading.
Preformatted

To create a preformatted section, begin each line with =. A preformatted section uses equally spaced text so that spacing is preserved.
Formatting

There is some sugar for font formatting:
To make something bold, surround it with ""
To make something italic, surround it with ''
To make something monospaced, surround it with ==
To make something strikethrough, surround it with --
To make something subscript, surround it with @@
To make something superscript, surround it with ^^
To make something underlined, surround it with __
Links

To create a link, put it between *. All links have the following form *reference*, or *alias>reference|parameters* for the complete form. The reference depends on the kind of link that is created (see below). The alias is a string being used as the text of the link, if left out a default is used. The parameters are optional parameters to further configure the behavior on the link, again this depends on the kind of link you create. The contents of some links, e.g. links pointing to image-files, downloads or other pages, can be embedded into the current document by putting the reference between + instead of *, for example +reference+. Not all types of links support embedding and quietly ignore it.
Internal Links

If a structure with the given name exists, a link to that item shows up when the page is saved. In case the path points to an non-existing structure, the user will be offered the possibility to create a new one when clicking on the link. The reference can be an absolute (/information/license) or relative (../license) path. The parameters command and view are supported to create specific links onto commands and views of the target page.
External Links

If the link is an URL (e.g. *http://www.lukas-renggli.ch*), a link to the external page shows up.
If the link is an e-mail address (e.g. *renggli@gmail.com*), a link to mail that person shows up.
Value Links
