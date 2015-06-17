%!TEX program = xelatex
\documentclass[12pt, compress, t]{beamer}
%include polycode.fmt
\usetheme[titleprogressbar]{m}
\usepackage{tikz-cd}       % Коммутативные диаграммы
\usepackage{booktabs}
\usepackage[scale=2]{ccicons}
\usepackage{minted}
\usepackage{listings}


\usepgfplotslibrary{dateplot}

\usemintedstyle{trac}

% \lstset{language=Haskell,
%         basicstyle=\footnotesize}

% Большие номера слайдов формата i/N
\setbeamerfont{page number in head/foot}{size=\large}
\setbeamertemplate{footline}[frame number]
% Большие номера слайдов формата i/N

% Команда для удобной вставки скриншота
\newcommand{\screenshotw}[2]{
  \centering\includegraphics[width=#1,keepaspectratio]{./images/#2}
}
% Команда для удобной вставки скриншота


% Форматирование <|>
%format <|> = "\mathbin{\raisebox{0.3ex}{$\scriptstyle < | >$}}"

\definecolor{cloud}{HTML}{ECF0F1}
\setbeamercolor{block title}{fg=white,bg=orange!75!black}
\setbeamercolor{block body}{fg=black,bg=cloud}

\title{Функциональный парсер легковесного языка разметки Markdown
на основе комбинирования монад и моноидального представления исходного текста}
\date{20.06.2015}
\author{Г.~А.~Лукьянов, ПМИ, группа 4.1 \\{Научный руководитель: асс. каф. ИВЭ А.~М.~Пеленицын}}
\institute{Институт математики, механики и компьютерных наук ЮФУ}

\renewcommand{\hscodestyle}{\small}

\begin{document}

\maketitle

\begin{frame}[fragile]
  \frametitle{Поставленные задачи}
  \begin{enumerate}
    \item Разработка двух библиотек \textbf{монадических парсеров}, 
    с применением различных технологий \textbf{комбинирования вычислительных 
    эффектов}. Обе библиотеки должны использовать \textbf{моноидальное}
    представление исходного текста.
    \item \textbf{Сравнение подходов} к комбинированию вычислительных эффектов, выявление их преимуществ и недостатков.
    \item Разработка \textbf{транслятора Markdown} с \LaTeX-вставками в HTML и \LaTeX.
  \end{enumerate}
\end{frame}

\begin{frame}[fragile]
  \frametitle{Использовались результаты работ: }
  \begin{description}
    \item [MParsers96]
    Monadic Parser Combinators // \textit{Graham Hutton}, \textit{Erik Meijer} –
Department of Computer Science, University of Nottingham, 1996
    \item [Monoids13]
    Adding Structure to Monoids // \textit{Mario Blaževic} – Stilo International plc – \\\textbf{Haskell Symposium 2013}
    \item [ExtEff13] 
    Extensible Effects An Alternative to Monad Transformers // \textit{Oleg
Kiselyov}, \textit{Amr Sabry}, \textit{Cameron Swords} – Indiana University, USA – \textbf{Haskell Symposium 2013}
  \end{description}
\end{frame}

\begin{frame}[fragile]
  \frametitle{Библиотеки монадических парсеров}
  \begin{itemize}
    \setlength\itemsep{2em}
    \item[] {\Large{Parsec}} \\
      \footnotesize{\url{https://hackage.haskell.org/package/parsec}}
    \item[] {\Large{Attoparsec}} \\
      \footnotesize{\url{https://hackage.haskell.org/package/attoparsec}}
    \item[] {\Large{Picoparsec}} \\
      \footnotesize{\url{https://hackage.haskell.org/package/picoparsec}}
  \end{itemize}
\end{frame}

\begin{frame}[fragile]
  \frametitle{Исходный тип для парсера~[MParsers96]}
  \begin{block}{Тип Parser}
    \begin{code}
newtype  Parser a = Parser { 
         parse :: String -> Maybe (a, String)}
    \end{code}
  \end{block}
  \begin{block}{Экземпляр класса типов Monad}
    \vspace{-1em}
  	\begin{code}
instance  Monad Parser where
          return t = Parser $ \s -> Just (t, s)
          m >>= k  = Parser $ \s -> 
                        do  (u, v)  <- parse m s
                            (x, y)  <- parse (k u) v
                            return (x, y)
  	\end{code}
  \end{block}
\end{frame}

\begin{frame}[fragile]
  \frametitle{Строковые типы в Haskell}
  \begin{itemize}
    \setlength\itemsep{2em}
    \item[] {\Large{String}} \\
      \small{Псевдоним для списка символов}
    \item[] {\Large{ByteString}} \\
      \small{Наиболее низкоуровневый тип}
    \item[] {\Large{Text}} \\
      \small{Тип для работы с Unicode-текстом}
  \end{itemize}
\end{frame}

\begin{frame}[fragile]
  \frametitle{Строковые типы как моноиды~[Monoids]}
    \begin{block}{Полиморфный по входу базовый парсер}
      \begin{code}
item :: TextualMonoid t => Parser t Char
item =  Parser f 
        where f inp = splitCharacterPrefix inp 
      \end{code}
    \end{block}
    \begin{block}{Фукнция, отделяющая префикс}
      \begin{code}
splitCharacterPrefix ::  TextualMonoid t => 
                         t -> Maybe (Char, t)
      \end{code}
    \end{block}
\end{frame}

\begin{frame}[fragile]
  \frametitle{Парсер как стек монад}
  \begin{block}{Обновленный тип Parser}
    \begin{code}
newtype  Parser t a = 
         Parser  (StateT (ParserState t) 
                 (Either (ErrorReport t)) a)
    \end{code}
  \end{block}
\end{frame}

\begin{frame}[fragile]
  \frametitle{Extensible Effects~[ExtEff13]}
  \begin{block}{Тип Eff}
    \begin{code}
type Eff r a = Free (Union r) a
    \end{code}
  \end{block}
  \begin{block}{Пример статического набора эффектов}
    \begin{center}
Eff (Reader Int :> Reader Bool :> Void) a
    \end{center}
  \end{block}
\end{frame}

\begin{frame}[fragile]
  \frametitle{Парсеры на Extensible Effects~[ExtEff13]}

  \begin{block}{Парсер-предикат и его эффекты}
    \begin{code}
sat ::  ( Member Fail r
        , Member (State String) r
        ) => (Char -> Bool) -> Eff r Char
sat p   = do  x <- item
              if p x then return x else die
    \end{code}
  \end{block}

  \begin{block}{Запуск парсера}
    \begin{code}
parse p inp = run . runFail . runState inp $ p
    \end{code}
  \end{block}
\end{frame}

\begin{frame}[fragile]
  \frametitle{Парсеры на Extensible Effects~[ExtEff13]}

  \begin{block}{Парсер для слов и его эффекты}
    \begin{code}
word ::  (Member Fail r
         , Member (State String) r
         , Member (Choose) r) => Eff r String
word = some letter
	  \end{code}
  \end{block}

  \begin{block}{Запуск парсера}
    \begin{code}
parseWithChoose p inp =
  run . runChoice . runFail . runState inp $ p
    \end{code}
  \end{block}
\end{frame}

\begin{frame}[fragile]
  \frametitle{Язык Markdown и его использование}
    \vspace{5em}
    \includegraphics[scale=1]{images/markdown.png}
    \includegraphics[scale=0.35]{images/octocat.png}
    \includegraphics[scale=0.35]{images/discourse.png}
\end{frame}

\begin{frame}[fragile]
  \frametitle{Пример документа Markdown}
  \vspace{0.5cm}
  \screenshotw{11cm}{md-html.png}
\end{frame}

\begin{frame}[fragile]
  \frametitle{AST для Markdown в Haskell}
  \begin{block}{Документ}
    \begin{code}
type Document = [Block]
    \end{code}
  \end{block}
  \begin{block}{Блок}
    \begin{code}
data  Block  = Blank
             | Header (Int,Line)
             | Paragraph [Line]
             | UnorderedList [Line]
             | BlockQuote [Line]
    \end{code}
  \end{block}
\end{frame}

\begin{frame}[fragile]
  \frametitle{AST для Markdown в Haskell}
  \begin{block}{Строка}
    \begin{code}
data Line = Empty | NonEmpty [Inline]    
    \end{code}
  \end{block}
  \begin{block}{Элементы строки}
    \begin{code}
data  Inline  = Plain String
              | Bold String
              | Italic String
              | Monospace String    
    \end{code}
  \end{block}
\end{frame}

\begin{frame}[fragile]
  \frametitle{Конструирование AST}
    \begin{block}{Парсер для документа}
      \vspace{-1.4em}
      \begin{code}
doc ::  TM.TextualMonoid t => Parser t Document
doc =   many block
        where block =  blank <|> header <|> paragraph 
                       <|> unorderdList <|> blockquote 
                       <|> blockMath
      \end{code}
      \vspace{-1.45em}
    \end{block}
    \begin{block}{Пример парсера для заголовка}
      \vspace{-1.4em}
      \begin{code}
header :: TM.TextualMonoid t => Parser t Block 
header =  do
          hashes  <- token $ some $ char '#' 
          text    <- nonEmptyLine
          return $ Header (length hashes,text)
      \end{code}
    \end{block}
\end{frame}

\begin{frame}[fragile]
  \frametitle{Результаты}
  \begin{enumerate}
    \item Разработана библиотека монадических парсеров, полиморфных по входным данным. Акцент сделан на подробность сообщений об ошибках. 
    \item Разработан транслятор подмножества Markdown с \LaTeX-вставками в HTML.
    \item Начата разработка библиотеки парсеров, основанных на Extensible Effects.
    \item Исходный код доступен в Git-репозиториях: \\
        \small{\url{https://github.com/geo2a/markdown_monparsing}} \\
        \small{\url{https://github.com/geo2a/ext-effects-parsers}}
  \end{enumerate}
\end{frame}

\end{document}