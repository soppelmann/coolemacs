;;; Compiled snippets and support files for `org-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'org-mode
                     '(("trampschooner"
                        "1. C-x C-f /ssh:bmooers@schooner.oscer.ou.edu\n2. enter password "
                        "Use tramp  to access schooner at OSCER" nil
                        ("protocols") nil
                        "/Users/getz/.emacs.d/snippets/org-mode/tramp-schooner"
                        nil nil)
                       ("tramppete"
                        "1. C-x C-f /ssh:bmooers@pete.hpc.okstate.edu\n2. enter password "
                        "Use tramp to access OSU cowboy supercomputer." nil
                        ("protocols") nil
                        "/Users/getz/.emacs.d/snippets/org-mode/tramp-pete" nil
                        nil)
                       ("trampxsede"
                        "1. C-x C-f /ssh:bmooers@expanse.sdsc.xsede.org\n2. enter password\n$0"
                        "Use tramp  to access Expanse at SDSC via xsede." nil
                        ("protocols") nil
                        "/Users/getz/.emacs.d/snippets/org-mode/tramp-expanse"
                        nil nil)
                       ("trampcluster"
                        "1. C-x C-f /ssh:mooers.hsc.net.ou.edu\n2. enter password "
                        "Use tramp  to access OUHSC gpu cluster." nil
                        ("protocols") nil
                        "/Users/getz/.emacs.d/snippets/org-mode/tramp-cluster"
                        nil nil)
                       ("title"
                        "#+TITLE: $1\n#+AUTHOR: $2\n#+EMAIL: $3\n#+DATE: $4\n#+LATEX_HEADER: \\usepackage[margin=1.0in]{geometry}\n#+OPTIONS: toc:nil email:nil\n#+cite_export: biblatex backend=bibtex,style=ieee,url=false\n#+BIBLIOGRAPHY: ~/Nextcloud/Zotero.bib\n\n$0"
                        "Org title" nil nil nil
                        "/Users/getz/.emacs.d/snippets/org-mode/title" nil nil)
                       ("ob-clojure-setup"
                        " 0. ~touch clojure.org~\n 1. ~lein new clojure-example~\n 2. ~cd clojure-example~\n 3. ~emacs~\n 4. ~C-x C-f clojure.org~\n 5. ~M-x cider-jack-in~\n 6. Wait for  message along the lines of  ~[nREPL] Direct connection to localhost:50367 established~.\n 7. Enter C-c C-c in the code block.\n "
                        "Setup for running clojure in org documents." nil
                        ("protocols") nil
                        "/Users/getz/.emacs.d/snippets/org-mode/protocol-ob-clojure-setup"
                        nil nil)
                       ("localGitPush"
                        " 0. ~git init~\n 1. ~git add --all~\n 2. ~git commit -m 'First commit'~\n 3. ~git remote add origin <remote repository URL>~\n 4. ~git remote -v~\n 5. ~git push origin master~\n  "
                        "Adding a local repo to GitHub." nil ("protocols") nil
                        "/Users/getz/.emacs.d/snippets/org-mode/protocol-add-local-repo-to-GitHub"
                        nil nil)
                       ("habit"
                        ":PROPERTIES:\n:STYLE: habit\n:LAST_REPEAT: [2022-04-09 Sat 14:26]\n:END:\n$0"
                        "habit (or repeated tasks)" nil ("properites") nil
                        "/Users/getz/.emacs.d/snippets/org-mode/org-property-habit"
                        nil nil)
                       ("cat" ":PROPERTIES:\n:CATEGORY: ${1:401}\n:END:\n$0"
                        "category" nil ("properites") nil
                        "/Users/getz/.emacs.d/snippets/org-mode/org-property-category"
                        nil nil)
                       ("table2csv"
                        "*** Section with table\n:PROPERTIES:\n:TABLE_EXPORT_FILE: ${1:change_me}.csv\n:TABLE_EXPORT_FORMAT: orgtbl-to-csv\n:END:\nThe file path leads to the directory of the current task.org file. \nPlace cursor in table and enter \"M-x org-table-export\" to send to a csv file.\nSource: https://emacs.stackexchange.com/questions/53723/export-table-to-csv-file-without-prompt\n$0"
                        "export table under point to" nil ("tables") nil
                        "/Users/getz/.emacs.d/snippets/org-mode/org-export-table"
                        nil nil)
                       ("elsrc2"
                        "#+BEGIN_SRC emacs-lisp :results value scalar\n${1:change}\n#+END_SRC\n$0"
                        "elisp source block with value scalar in header required to get ouput results block"
                        nil ("codeBlocks") nil
                        "/Users/getz/.emacs.d/snippets/org-mode/org-code-block-elisp"
                        nil nil)
                       ("sniptemp"
                        ";; Remember to edit the four fields above.\n${1:Enter code here}\n$0\n\n"
                        "template for new snippets" nil ("snippetTemplates") nil
                        "/Users/getz/.emacs.d/snippets/org-mode/newsnip" nil nil)
                       ("listplain"
                        "+ ${1:item}\n+ ${2:item}\n+ ${3:item}\n+ ${4:item}\n+ ${5:item}\n$0"
                        "plan list 5 items" nil ("lists") nil
                        "/Users/getz/.emacs.d/snippets/org-mode/listplain" nil
                        nil)
                       ("listnum"
                        "1. ${1:item}\n2. ${2:item}\n3. ${3:item}\n4. ${4:item}\n5. ${5:item}\n$0"
                        "numbered list 5 items" nil ("lists") nil
                        "/Users/getz/.emacs.d/snippets/org-mode/listnumb" nil
                        nil)
                       ("weblink" "[[${1:changeURL}] [${2:changeName}]]\n$0\n\n"
                        "insert weblink template in an org file" nil ("links")
                        nil
                        "/Users/getz/.emacs.d/snippets/org-mode/links-weblink"
                        nil nil)
                       ("verbatim"
                        "\\begin{verbatim}\n${1:Paste here}\n\\end{verbatim}\n$0\n"
                        "LaTeX code for verbatim env." nil ("latex-envs") nil
                        "/Users/getz/.emacs.d/snippets/org-mode/latex-env-verbatim"
                        nil nil)
                       ("3col"
                        "\\begin{table}\n\\begin{tabular}{lcc}\n\\begin{center}\n\\hline\n\\textbf{${1:Features}} & \\textbf{${2:Features}} & \\textbf{${3:Features}}\\\\\n\\hlin2\n${4:T} & ${5:dddd} & ${6:ddd} \\\\\n\\hline\n\\end{tabular}\n\\end{center}\n\\caption{${7:RET kinase.}}\n\\end{table}\n$0\n"
                        "Latex for figure env." nil ("latex-env") nil
                        "/Users/getz/.emacs.d/snippets/org-mode/latex-env-table-col3"
                        nil nil)
                       ("quote"
                        "\\begin{quote}\n${1:Paste here}\n\\end{quote}\n$0\n"
                        "LaTeX code for quote env" nil ("latex-envs") nil
                        "/Users/getz/.emacs.d/snippets/org-mode/latex-env-quote"
                        nil nil)
                       ("mintinpython"
                        "\\mintinline{python}{${1:replaceMe}} ${0}"
                        "Latex for python in mintinline." nil ("latex-envs") nil
                        "/Users/getz/.emacs.d/snippets/org-mode/latex-env-mintinpython"
                        nil nil)
                       ("mintinjulia" "\\mintinline{julia}{${1:replaceMe}} ${0}"
                        "Latex for Julia in mintinline." nil ("latex-envs") nil
                        "/Users/getz/.emacs.d/snippets/org-mode/latex-env-mintinjulia"
                        nil nil)
                       ("mintinjs"
                        "\\mintinline{JavaScript}{${1:replaceMe}} ${0}\n"
                        "Latex for mintinline with JavaScript language preset."
                        nil ("latex-envs") nil
                        "/Users/getz/.emacs.d/snippets/org-mode/latex-env-mintinjs"
                        nil nil)
                       ("mintinelisp" "\\mintinline{elisp}{${1:replaceMe}} ${0}"
                        "Latex for elisp in the mintinline." nil ("latex-envs")
                        nil
                        "/Users/getz/.emacs.d/snippets/org-mode/latex-env-mintinelisp"
                        nil nil)
                       ("mintineclj"
                        "\\mintinline{clojure}{${1:replaceMe}} ${0}\n"
                        "Latex for clojure language preset in mintinline." nil
                        ("latex-envs") nil
                        "/Users/getz/.emacs.d/snippets/org-mode/latex-env-mintinclojure"
                        nil nil)
                       ("mintinR" "\\mintinline{R}{${1:someRcode}} ${0}"
                        "Latex mintinline with R language present" nil
                        ("latex-envs") nil
                        "/Users/getz/.emacs.d/snippets/org-mode/latex-env-mintinR"
                        nil nil)
                       ("latex-env-mintinCpp"
                        "# -*- mode: snippet -*-\n# contributor: Blaine Mooers, bmooers1@gmail.com, github.com/MooersLab\n# name: Latex for mintinline with C++ language preset.\n# group: latex-envs\n# key: mintinCpp\n# -- mintinCpp\n\\mintinline{C++}{${1:replaceMe}}${0}"
                        "latex-env-mintinCpp" nil nil nil
                        "/Users/getz/.emacs.d/snippets/org-mode/latex-env-mintinCpp"
                        nil nil)
                       ("latex-env-mintinC"
                        "# -*- mode: snippet -*-\n# contributor: Blaine Mooers, bmooers1@gmail.com, github.com/MooersLab\n# name: Latex for mintinline with C language preset.\n# group: latex-envs\n# key: mintinC\n# -- mintinC\n\\mintinline{C}{${1:replaceMe}} ${0}\n"
                        "latex-env-mintinC" nil nil nil
                        "/Users/getz/.emacs.d/snippets/org-mode/latex-env-mintinC"
                        nil nil)
                       ("fig"
                        "\\begin{figure}\n\\begin{center}\n    \\includegraphics[width=0.86\\textwidth, angle=0]{${1:/Users/blaine/7JU5A.png}}\n\\end{center}\n\\caption{${2:RET kinase, PDB-ID: 7JU5}}\n\\end{figure}\n$0\n"
                        "Latex for figure env" nil ("latex-envs") nil
                        "/Users/getz/.emacs.d/snippets/org-mode/latex-env-fig"
                        nil nil)
                       ("eqc"
                        "% Add to preamble to create captioned equation environment.\n% \\usepackage{caption};\n%\\DeclareCaptionType{eqc}[][]\n%\\captionsetup[eqc]{labelformat=empty}\n\\label{eq:${1:nameOfEquation}}\n\\index{${2:nameOfEquation}}\n\\begin{eqc}\n${3:enter equation here}\n\\caption{${4:Caption of the equation}}\n\\end{eqc}\n${0}"
                        "Latex for captioned equation that is labeled and indexed."
                        nil ("latex-envs") nil
                        "/Users/getz/.emacs.d/snippets/org-mode/latex-env-eqcaptioned"
                        nil nil)
                       ("code4"
                        "% Line numbering on and aligned with left margin. \n% Add to preamble: \\newenvironment{code}{\\captionsetup{type=listing}}{}\n\\iffalse\n\\begin{code}{}  % (fold)\n\\index{${2:copying and pasting code!lost bytes}}\n\\label{lst:${3:redirScript}}\n\\caption{${4:Python script to find lost bytes.}}\n\\begin{minted}[frame=lines,\nframerule=2pt,\nlinenos=false,\nxleftmargin=\\parindent,\nbreaklines]{bash}\n\\fi\n\\begin{verbatim}\n${5:code listing}\n\\end{verbatim}\n\\iffalse    \n\\end{minted}\n\\end{code}\n\\fi\n$0"
                        "Latex for code environment that is captioned, labeled, indexed, and in comment block."
                        nil ("latex-envs") nil
                        "/Users/getz/.emacs.d/snippets/org-mode/latex-env-code4"
                        nil nil)
                       ("code3"
                        "% Line numbering on and aligned with left margin. \n% Add to preamble: \\newenvironment{code}{\\captionsetup{type=listing}}{}\n\\begin{code}{} % (fold)\n\\label{lst:${1:label}}\n\\index{${2:copying and pasting code!lost bytes}}\n\\caption{${3:Python script to find lost bytes.}}\n\\begin{minted}[frame=lines,framerule=2pt]{python}\n${4:code listing}\n\\end{minted}\n\\end{code}\n% listing (end)\n$0"
                        "Latex for code environment that is captioned, labeled, and indexed."
                        nil ("latex-envs") nil
                        "/Users/getz/.emacs.d/snippets/org-mode/latex-env-code3"
                        nil nil)
                       ("code2"
                        "% Line numbering on and aligned with left margin. \n% Add to preamble: \\newenvironment{code}{\\captionsetup{type=listing}}{}\n\\begin{code}{}\n\\index{${1:openCV}!${2:measureSizes}}\n\\label{lst:${3:measureSize}}\n\\caption{${4:Contents of measureSizes.py.}}\n\\begin{minted}[frame=lines,\n               framerule=2pt,\n               linenos=true,\n               xleftmargin=\\parindent,\n               breaklines]{python}\n${5:import me}\n\\end{minted}\n\\end{code}\n${0}\n"
                        "Latex for code environment that is captioned, labeled, and indexed."
                        nil ("latex-envs") nil
                        "/Users/getz/.emacs.d/snippets/org-mode/latex-env-code2"
                        nil nil)
                       ("code1"
                        "% Line numbering on and aligned with left margin. \n% Add to preamble: \\newenvironment{code}{\\captionsetup{type=listing}}{}\n\\begin{code}{}\n\\index{${1:openCV}!${2:measureSizes}}\n\\label{lst:${3:measureSize}}\n\\caption{${4:Contents of measureSizes.py.}}\n\\inputminted[frame=lines,\n               framerule=2pt,\n               linenos=true,\n               xleftmargin=\\parindent,\n               breaklines]{python}{${5:./Content/codeListings/test.py}}\n\\end{minted}\n\\end{code}\n$0"
                        "Latex for code environment that is captioned, labeled, indexed, line numbered, and left aligned."
                        nil ("latex-envs") nil
                        "/Users/getz/.emacs.d/snippets/org-mode/latex-env-code1"
                        nil nil)
                       ("code0"
                        "% Line numbering on and aligned with left margin. \n% Add to preamble: \\newenvironment{code}{\\captionsetup{type=listing}}{}\n\\begin{code}{}\n\\index{${1:openCV}!${2:measureSizes}}\n\\label{lst:${3:measureSize}}\n\\caption{${4:Contents of measureSizes.py.}}\n\\begin{minted}[frame=lines,\n               framerule=2pt,\n               linenos=true,\n               xleftmargin=\\parindent,\n               breaklines]{python}\n${5:import me}\n\\end{minted}\n\\end{code}\n$0\n"
                        "Latex for code environment that is captioned, labeled, and indexed."
                        nil ("latex-envs") nil
                        "/Users/getz/.emacs.d/snippets/org-mode/latex-env-code0"
                        nil nil)
                       ("jupyterPyMOL"
                        "#+BEGIN_SRC jupyter-python :session py :kernel ${1:cctbxpymol38} :exports both :results raw drawer\nfrom pymol import cmd\ncmd.do(\"reinitialize\")\ncmd.bg_color(\"white\")\ncmd.do(\"fetch ${2:6VXX}\")\ncmd.do(\"zoom (resi 614 and chain A)\")\ncmd.label(selection=\"chain A and resi 614 and name CB\", expression=\"'%s-%s' % (resn,resi)\")\ncmd.do(\"set label_color, black; set label_size, 48\")\ncmd.do(\"set stick_radius, 0.12\")\ncmd.do(\"hide cartoon; show sticks\")\ncmd.do(\"set ray_shadows, 0\")\ncmd.do(\"draw\")\ncmd.do(\"png /Users/blaine/${2:6VXX}.png, 600, 360, dpi=600\")\nfrom IPython.display import Image\nfrom IPython.core.display import HTML\nPATH = \"/Users/blaine/\"\nImage(filename = PATH + \"${2:6VXX}.png\", width=600, unconfined=True)\n#+END_SRC\n$0"
                        "source block frontends to Jupyter kernels using emacs-jupyter"
                        nil ("code blocks") nil
                        "/Users/getz/.emacs.d/snippets/org-mode/jupyterPyMOL"
                        nil nil)
                       ("jupyter"
                        "#+BEGIN_SRC jupyter-python :session py :kernel ${1:cp38} :exports both :results raw drawer\n${2:change me}\n#+END_SRC\n$0"
                        "source block frontends to Jupyter kernels using emacs-jupyter"
                        nil ("code blocks") nil
                        "/Users/getz/.emacs.d/snippets/org-mode/jupyter" nil nil)
                       ("table2csv"
                        "*** Section with table\n:PROPERTIES:\n:TABLE_EXPORT_FILE: ${1:change_me}.csv\n:TABLE_EXPORT_FORMAT: orgtbl-to-csv\n:END:\nThe file path leads to the directory of the current task.org file. \nPlace cursor in table and enter \"M-x org-table-export\" to send to a csv file.\nSource: https://emacs.stackexchange.com/questions/53723/export-table-to-csv-file-without-prompt\n$0"
                        "export table under point to" nil ("tables") nil
                        "/Users/getz/.emacs.d/snippets/org-mode/export-org-table"
                        nil nil)
                       ("date" "`(format-time-string \"%Y-%m-%d\")`$0" "date"
                        nil nil nil
                        "/Users/getz/.emacs.d/snippets/org-mode/date" nil nil)
                       ("dh2"
                        "#+TITLE: ${1:20211019} -*- mode: org; -*-\n#+STARTUP: showall\n#+STARTUP: noindent\n#+LATEX_HEADER: \\usepackage[margin=0.75in]{geometry}\n\n** To do list\n\n*** urgent and important, quick     \n - [ ] Update gcal\n - [ ] \n - [ ] \n\n*** urgent and important, slow    \n - [ ] \n - [ ] \n - [ ] \n\n*** important not urgent, slow    \n - [ ] \n - [ ] \n - [ ] \n\n$0"
                        "daily header for diary file with to do list" nil
                        ("todoList") nil
                        "/Users/getz/.emacs.d/snippets/org-mode/dailyHeader2"
                        nil nil)
                       ("dh"
                        "#+TITLE: ${1:20211019} -*- mode: org; -*-\n#+STARTUP: showall\n#+STARTUP: noindent\n#+LATEX_HEADER: \\usepackage[margin=0.75in]{geometry}\n$0"
                        "dailyHeader" nil nil nil
                        "/Users/getz/.emacs.d/snippets/org-mode/dailyHeader" nil
                        nil)
                       ("srcblock"
                        "#+NAME: ${1:name}\n#+BEGIN_SRC ${2:language}\n  $3\n#+END_SRC\n"
                        "srcblock" nil nil nil
                        "/Users/getz/.emacs.d/snippets/org-mode/code" nil nil)
                       ("c5"
                        "- [ ] ${1:item}\n- [ ] ${2:item}\n- [ ] ${3:item}\n- [ ] ${4:item}\n- [ ] ${5:item}\n$0"
                        "list of 5 checkbox items" nil ("lists") nil
                        "/Users/getz/.emacs.d/snippets/org-mode/checklist5" nil
                        nil)))


;;; Do not edit! File generated at Tue Feb 18 13:41:18 2025
