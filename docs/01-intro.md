# Introduction {#intro}

In progress. 
But for now, in case anyone manages to stumble across this repository, it is probably good to know a few things before you read ahead:

1. This transcription hasn't been proofread just yet so please do put on your critical thinking hats and evaluate the content. 
You can also [submit any issues as needed](https://github.com/MokeEire/BookdownISL/issues).
2. I am attempting to stay completely faithful to the text. 
It cannot be understated -- **this is not my work**, I am merely transcribing and learning. 
In a bid to use the authors' formatting, words in orange/red are related to `data`.
In attempting to bring the user-friendliness of online books to the material, words in blue indicate <strong><span style='font-family:monospace; color: #1188ce;'>keywords</span></strong>.
In the book, these words are italicized and marked in the margins of the page.

## An Overview of Statistical Learning

*Statistical learning* refers to a vast set of tools for *understanding data*.
These tools can be classified as *supervised* or *unsupervised.* Broadly speaking, supervised statistical learning involves building a statistical model for predicting, or estimating, an *output* based on one or more *inputs.*
Problems of this nature occur in fields as diverse as business, medicine, astrophysics, and public policy. 
With unsupervised statistical learning, there are inputs but
no supervising output; nevertheless we can learn relationships and structure from such data. 
To provide an illustration of some applications of statistical learning, we briefly discuss three real-world data sets that are considered in this book.

### Wage Data

In this application (which we refer to as the `Wage` data set throughout this book), we examine a number of factors that relate to wages for a group of males from the Atlantic region of the United States. 
In particular, we wish to understand the association between an employee’s `age` and `education`, as well as the calendar `year`,on his `wage`. Consider, for example, the left-hand panel of Figure 1.1, which displays `wage` versus `age` for each of the individuals in the data set. 
There is evidence that `wage` increases with `age` but then decreases again after approximately age 60.
The blue line, which provides an estimate of the average `wage` for a given `age`, makes this trend clearer.

FIGURE 1.1

Given an employee’s `age`, we can use this curve to *predict* his `wage`. However, it is also clear from Figure 1.1 that there is a signiﬁcant amount of variability associated with this average value, and so `age` alone is unlikely to provide an accurate prediction of a particular man’s `wage`.

We also have information regarding each employee’s education level and the year in which the `wage` was earned. 
The center and right-hand panels of Figure 1.1, which display `wage` as a function of both `year` and `education`, indicate that both of these factors are associated with `wage`.
Wages increase by approximately $10,000, in a roughly linear (or straight-line) fashion, between 2003 and 2009, though this rise is very slight relative to the variability in the data. 
Wages are also typically greater for individuals with higher education levels: men with the lowest education level (1) tend to
have substantially lower wages than those with the highest education level (5). 
Clearly, the most accurate prediction of a given man’s `wage` will be obtained by combining his `age`, his `education`,and the `year`.
In Chapter 3, we discuss linear regression, which can be used to predict `wage` from this data set. 
Ideally, we should predict `wage` in a way that accounts for the non-linear relationship between `wage` and `age`. 
In Chapter 7, we discuss a class of approaches for addressing this problem.

### Stock Market Data

The `Wage` data involves predicting a *continuous* or *quantitative* output value.
This is often referred to as a *regression* problem. 
However, in certain cases we may instead wish to predict a non-numerical value—that is, a *categorical* or *qualitative* output. 
For example, in Chapter 4 we examine a stock market data set that contains the daily movements in the Standard & Poor’s 500 (S&P) stock index over a 5-year period between 2001 and 2005. 
We refer to this as the `Smarket` data. 
The goal is to predict whether the index will *increase* or *decrease* on a given day using the past 5 days’ percentage changes in the index. 
Here the statistical learning problem does not involve predicting a numerical value. 
Instead it involves predicting whether a given day’s stock market performance will fall into the `Up` bucket or the `Down` bucket. 
This is known as a *classiﬁcation* problem. 
A model that could accurately predict the direction in which the market will move would be very useful!

FIGURE 1.2

The left-hand panel of Figure 1.2 displays two boxplots of the previous
day’s percentage changes in the stock index: one for the 648 days for which the market increased on the subsequent day, and one for the 602 days for which the market decreased. 
The two plots look almost identical, suggesting that there is no simple strategy for using yesterday’s movement in the S&P to predict today’s returns. 
The remaining panels, which display boxplots for the percentage changes 2 and 3 days previous to today, similarly indicate little association between past and present returns. 
Of course, this lack of pattern is to be expected: in the presence of strong correlations between successive days’ returns, one could adopt a simple trading strategy to generate profits from the market.
Nevertheless, in Chapter 4, we explore these data using several different statistical learning methods. 
Interestingly, there are hints of some weak trends in the data that suggest that, at least for this 5-year period, it is possible to correctly predict the direction of movement in the market approximately 60% of the time (Figure 1.3).

FIGURE 1.3

### Gene Expression Data

The previous two applications illustrate data sets with both input and output variables. 
However, another important class of problems involves situations in which we only observe input variables, with no corresponding output. 
For example, in a marketing setting, we might have demographic information for a number of current or potential customers. 
We may wish to understand which types of customers are similar to each other by grouping individuals according to their observed characteristics. 
This is known as a *clustering* problem. 
Unlike in the previous examples, here we are not trying to predict an output variable.

We devote Chapter 10 to a discussion of statistical learning methods for problems in which no natural output variable is available. 
We consider the <strong><span style='font-family:monospace; color: #B44C1C;'>NCI60</span></strong> data set, which consists of $6,830$ gene expression measurements for each of 64 cancer cell lines. 
Instead of predicting a particular output variable, we are interested in determining whether there are groups, or clusters, among the cell lines based on their gene expression measurements.
This is a diﬃcult question to address, in part because there are thousands of gene expression measurements per cell line, making it hard to visualize the data.

The left-hand panel of Figure 1.4 addresses this problem by representing each of the 64 cell lines using just two numbers, $Z_1$
and $Z_2$.
These are the first two *principal components* of the data, which summarize the $6,830$ expression measurements for each cell line down to two numbers or *dimensions.* 
While it is likely that this dimension reduction has resulted in some loss of information, it is now possible to visually examine the data for evidence of clustering. 
Deciding on the number of clusters is often a difficult problem. 
But the left-hand panel of Figure 1.4 suggests at least four groups of cell lines, which we have represented using separate colors. 
We can now examine the cell lines within each cluster for similarities in their types of cancer, in order to better understand the relationship between gene expression levels and cancer.

In this particular data set, it turns out that the cell lines correspond to 14 different types of cancer. (However, this information was not used to create the left-hand panel of Figure 1.4.) 
The right-hand panel of Figure 1.4 is identical to the left-hand panel, except that the 14 cancer types are shown using distinct colored symbols. 
There is clear evidence that cell lines with the same cancer type tend to be located near each other in this two-dimensional representation.
In addition, even though the cancer information was not used to produce the left-hand panel, the clustering obtained does bear some resemblance to some of the actual cancer types observed in the right-hand panel.
This provides some independent verification of the accuracy of our clustering analysis.

## A Brief History of Statistical Learning

Though the term *statistical learning* is fairly new, many of the concepts that underlie the ﬁeld were developed long ago. 
At the beginning of the nineteenth century, Legendre and Gauss published papers on the *method of least squares*, which implemented the earliest form of what is now known as *linear regression*. 
The approach was first successfully applied to problems in astronomy.
Linear regression is used for predicting quantitative values, such as an individual’s salary. 
In order to predict qualitative values, such as whether a patient survives or dies, or whether the stock market increases or decreases, Fisher proposed *linear discriminant analysis* in 1936. 
In the 1940s, various authors put forth an alternative approach, *logistic regression*.
In the early 1970s, Nelder and Wedderburn coined the term *generalized linear models* for an entire class of statistical learning methods that include both linear and logistic regression as special cases.

By the end of the 1970s, many more techniques for learning from data were available. 
However, they were almost exclusively *linear* methods, because fitting *non-linear* relationships was computationally infeasible at the time. 
By the 1980s, computing technology had finally improved sufficiently that non-linear methods were no longer computationally prohibitive. 
In mid 1980s Breiman, Friedman, Olshen and Stone introduced *classification and regression trees*, and were among the first to demonstrate the power of a detailed practical implementation of a method, including cross-validation for model selection. 
Hastie and Tibshirani coined the term *generalized additive models* in 1986 for a class of non-linear extensions to generalized linear
models, and also provided a practical software implementation.

Since that time, inspired by the advent of *machine learning* and other disciplines, statistical learning has emerged as a new subfield in statistics, focused on supervised and unsupervised modeling and prediction. 
In recent years, progress in statistical learning has been marked by the increasing availability of powerful and relatively user-friendly software, such as the popular and freely available `R` system. 
This has the potential to continue the transformation of the field from a set of techniques used and developed by statisticians and computer scientists to an essential toolkit for a much broader community.

## This Book

*The Elements of Statistical Learning* (ESL) by Hastie, Tibshirani, and Friedman was first published in 2001. 
Since that time, it has become an important reference on the fundamentals of statistical machine learning.
Its success derives from its comprehensive and detailed treatment of many important topics in statistical learning, as well as the fact that (relative to many upper-level statistics textbooks) it is accessible to a wide audience.
However, the greatest factor behind the success of ESL has been its topical nature. 
At the time of its publication, interest in the field of statistical learning was starting to explode. 
ESL provided one of the first accessible and comprehensive introductions to the topic.

Since ESL was first published, the field of statistical learning has continued to flourish. 
The field’s expansion has taken two forms. 
The most obvious growth has involved the development of new and improved statistical learning approaches aimed at answering a range of scientific questions across a number of fields. 
However, the field of statistical learning has also expanded its audience. 
In the 1990s, increases in computational power generated a surge of interest in the field from non-statisticians who were eager to use cutting-edge statistical tools to analyze their data. 
Unfortunately, the highly technical nature of these approaches meant that the user community remained primarily restricted to experts in statistics, computer science, and related fields with the training (and time) to understand and implement them.

In recent years, new and improved software packages have significantly eased the implementation burden for many statistical learning methods.
At the same time, there has been growing recognition across a number of fields, from business to health care to genetics to the social sciences and beyond, that statistical learning is a powerful tool with important practical applications. 
As a result, the field has moved from one of primarily academic interest to a mainstream discipline, with an enormous potential audience.
This trend will surely continue with the increasing availability of enormous quantities of data and the software to analyze it.

The purpose of *An Introduction to Statistical Learning* (ISL) is to facilitate the transition of statistical learning from an academic to a mainstream field. 
ISL is not intended to replace ESL, which is a far more comprehensive text both in terms of the number of approaches considered and the depth to which they are explored. 
We consider ESL to be an important companion for professionals (with graduate degrees in statistics, machine learning, or related fields) who need to understand the technical details behind statistical learning approaches. 
However, the community of users of statistical learning techniques has expanded to include individuals with a wider range of interests and backgrounds. 
Therefore, we believe that there is now a place for a less technical and more accessible version of ESL.

In teaching these topics over the years, we have discovered that they are of interest to master’s and PhD students in fields as disparate as business administration, biology, and computer science, as well as to quantitatively oriented upper-division undergraduates. 
It is important for this diverse group to be able to understand the models, intuitions, and strengths and weaknesses of the various approaches. 
But for this audience, many of the technical details behind statistical learning methods, such as optimization algorithms and theoretical properties, are not of primary interest.
We believe that these students do not need a deep understanding of these aspects in order to become informed users of the various methodologies, and in order to contribute to their chosen fields through the use of statistical learning tools.

ISLR is based on the following four premises.

1. *Many statistical learning methods are relevant and useful in a wide range of academic and non-academic disciplines, beyond just the statistical sciences*. 
We believe that many contemporary statistical learning procedures should, and will, become as widely available and used as is currently the case for classical methods such as linear regression. 
As a result, rather than attempting to consider every possible approach (an impossible task), we have concentrated on presenting the methods that we believe are most widely applicable.

2. *Statistical learning should not be viewed as a series of black boxes*. 
No single approach will perform well in all possible applications.
Without understanding all of the cogs inside the box, or the interaction between those cogs, it is impossible to select the best box. 
Hence, we have attempted to carefully describe the model, intuition, assumptions, and trade-offs behind each of the methods that we consider.

3. *While it is important to know what job is performed by each cog, it is not necessary to have the skills to construct the machine inside the box!* 
Thus, we have minimized discussion of technical details related to fitting procedures and theoretical properties. 
We assume that the reader is comfortable with basic mathematical concepts, but we do not assume a graduate degree in the mathematical sciences. 
For instance, we have almost completely avoided the use of matrix algebra, and it is possible to understand the entire book without a detailed knowledge of matrices and vectors.

4. *We presume that the reader is interested in applying statistical learning methods to real-world problems.* 
In order to facilitate this, as well as to motivate the techniques discussed, we have devoted a section within each chapter to `R` computer labs. 
In each lab, we walk the reader through a realistic application of the methods considered in that chapter. 
When we have taught this material in our courses, we have allocated roughly one-third of classroom time to working through the labs, and we have found them to be extremely useful.
Many of the less computationally-oriented students who were initially intimidated by `R`’s command level interface got the hang of things over the course of the quarter or semester. 
We have used `R` because it is freely available and is powerful enough to implement all of the methods discussed in the book.
It also has optional packages that can be downloaded to implement literally thousands of additional methods. 
Most importantly, `R` is the language of choice for academic statisticians, and new approaches often become available in `R` years before they are implemented in commercial packages. 
However, the labs in ISL are self-contained, and can be skipped if the reader wishes to use a different software package or does not wish to apply the methods discussed to real-world problems.

## Who Should Read This Book

This book is intended for anyone who is interested in using modern statistical methods for modeling and prediction from data. 
This group includes scientists, engineers, data analysts, or *quants*, but also less technical individuals with degrees in non-quantitative fields such as the social sciences or business. 
We expect that the reader will have had at least one elementary course in statistics. 
Background in linear regression is also useful, though not required, since we review the key concepts behind linear regression in Chapter 3.
The mathematical level of this book is modest, and a detailed knowledge of matrix operations is not required. 
This book provides an introduction to the statistical programming language `R`. 
Previous exposure to a programming language, such as `MATLAB` or `Python`, is useful but not required.

We have successfully taught material at this level to master’s and PhD students in business, computer science, biology, earth sciences, psychology, and many other areas of the physical and social sciences. This book could also be appropriate for advanced undergraduates who have already taken a course on linear regression. 
In the context of a more mathematically rigorous course in which ESL serves as the primary textbook, ISL could be used as a supplementary text for teaching computational aspects of the various approaches.

## Notation and Simple Matrix Algebra

Choosing notation for a textbook is always a difficult task. 
For the most part we adopt the same notational conventions as ESL.
We will use *n* to represent the number of distinct data points, or observations, in our sample. 
We will let *p* denote the number of variables that are available for use in making predictions. 
For example, the `Wage` data set consists of 12 variables for $3,000$ people, so we have $n =3,000$ observations and
$p = 12$ variables (such as `year`, `age`, `wage`, and more). 
Note that throughout this book, we indicate variable names using colored font: `Variable Name`.
In some examples, *p* might be quite large, such as on the order of thousands or even millions; this situation arises quite often, for example, in the analysis of modern biological data or web-based advertising data.

In general, we will let $x_{ij}$ represent the value of the *j*th variable for the *i*th observation, where $i =1, 2,...,n$ and $j =1, 2,...,p$. 
Throughout this book, *i* will be used to index the samples or observations (from 1 to *n*) and *j* will be used to index the variables (from 1 to *p*). 
We let _**X**_ denote a $n \times p$ matrix whose (*i, j*)th element is $x_{ij}$. 
That is,

$$
X = \begin{pmatrix}
x_{11} & x_{12} & \cdots & x_{1p} \\
x_{21} & x_{22} & \cdots & x_{2p} \\
\vdots & \vdots & \ddots & \vdots \\
x_{n1} & x_{n2} & \cdots & x_{np} \\
\end{pmatrix}
$$

For readers who are unfamiliar with matrices, it is useful to visualize _**X**_ as a spreadsheet of numbers with *n* rows and *p* columns.

At times we will be interested in the rows of _**X**_, which we write as $x_1,x_2,...,x_n$.
Here *x* is a vector of length *p*, containing the *p* variable measurements for the *i*th observation. 
That is,

\begin{equation}

x_i = \begin{pmatrix}
x_{i1}\\
x_{i2}\\
\vdots\\
x_{ip}\\
\end{pmatrix}
(\#eq:row-vector)
\end{equation}

(Vectors are by default represented as columns.) 
For example, for the Wage data, $x_i$ is a vector of length 12, consisting of year, age, wage, and other values for the *i*th individual. 
At other times we will instead be interested in the columns of _**X**_, which we write as $x_1, x_2, \dots, x_p$.
Each is a vector of length *n*. 
That is,

$$
x_i = \begin{pmatrix}
x_{1j}\\
x_{2j}\\
\vdots\\
x_{nj}\\
\end{pmatrix}
$$
For example, for the `Wage` data, $x_1$ contains the $n=3,000$ values for `year`.
Using this notation, the matrix _**X**_ can be written as

$$
X = \begin{pmatrix}
x_1 & x_2 & \cdots & x_p
\end{pmatrix},
$$
or

$$
X = \begin{pmatrix}
x^T_1 \\ x^T_2 \\ \vdots \\ x^T_n
\end{pmatrix}.
$$
The $^T$ notation denotes the *transpose* of a matrix or vector.
So, for example,

$$
X^T = \begin{pmatrix}
x_11 & x_21 & \cdots & x_n1 \\
x_12 & x_22 & \cdots & x_n2 \\
\vdots & \vdots & & \vdots \\
x_1p & x_2p & \cdots & x_np 
\end{pmatrix}
$$
while

$$
x^T_i = \begin{pmatrix}
x_{i1} & x_{i2} & \cdots & x_ip
\end{pmatrix}.
$$

We use $y_i$ to denote the *i*th observation of the variable on which we wish to make predictions such as `wage`.
Hence, we write the set of all *n* observations in vector form as 

$$
y = \begin{pmatrix}
y_1 \\ y_2 \\ \vdots \\ y_n
\end{pmatrix}
$$
Then our observed data consists of ${(x_1, y_1), (x_2, y_2), \dots, (x_n, y_n)}$, where each $x_i$ is a vector of length *p*.
(If $p=1$, then $x_1$ is simply a scalar.)

In this text, a vector of length *n* will always be denoted in *lower case bold*; e.g.

$$
\textbf{a} = \begin{pmatrix}
a_1 \\ a_2 \\ \vdots \\ a_n
\end{pmatrix}
$$
However, vectors that are not of length *n* (such as feature vectors of length *p*, as in \@ref(eq:row-vector)) will be denoted in *lower case normal font*, e.g. *a*.
Scalars will also be denoted in *lower case normal font* e.g. *a*.
In the rare cases in which these two uses for lower case normal font lead to ambiguity, we will clarify which use is intended.
Matrices will be denoted using *bold capitals*, such as _**A**_.
Random variables will be denoted using *capital normal font*, e.g. *A*, regardless of their dimensions.

Occasionally we will want to indicate the dimension of a particular object.
To indicate that an object is a scalar, we will use the notation $a \in \mathbb{R}$.
To indicate that it is a vector of length *k*, we will use $a \in \mathbb{R}^k$ (or $\textbf{a} \in \mathbb{R}^n$ if it is of length *n*).
We will indicate that an object is a $r \times s$ matrix using $\textbf{A} \in \mathbb{R}^{r\times s}$.


We have avoided using matrix algebra whenever possible. 
However, in a few instances it becomes too cumbersome to avoid it entirely. 
In these rare instances it is important to understand the concept of multiplying two matrices. 
Suppose that $\textbf{A} \in \mathbb{R}^{r \times d}$ and $\textbf{B} \in \mathbb{R}^{r \times s}$.
Then the product of _**A**_ and _**B**_ is denoted _**AB**_.
The (*i, j*)th element of _**AB**_ is computed by multiplying each element of the *i*th row of _**A**_ by the corresponding element of the *j*th column of _**B**_.
That is, $(\textbf{AB})_{ij} =  \sum_{k=1}^{d} a_{ik}b_{kj}$.
As an example, consider

$$\textbf{A} = \begin{pmatrix}
1 & 2 \\
3 & 4 \\
\end{pmatrix}
\ \ \ 
and 
\ \ \
\textbf{B} = \begin{pmatrix}
5 & 6 \\
7 & 8 \\
\end{pmatrix}
$$
Then

$$
\textbf{AB} = \begin{pmatrix}
1 & 2 \\
3 & 4 \\
\end{pmatrix}
\begin{pmatrix}
5 & 6 \\
7 & 8 \\
\end{pmatrix}
= 
\begin{pmatrix}
1 \times 5 + 2 \times 7 & 1 \times 6 + 2 \times 8 \\
3 \times 5 + 4 \times 7 & 3 \times 6 + 4 \times 8 \\
\end{pmatrix}
= 
\begin{pmatrix}
19 & 22 \\
43 & 50 \\
\end{pmatrix}
$$
Note that this operation produces an $r \times s$ matrix.
It is only possible to compute $\textbf{AB}$ if the number of columns of _**A**_ is the same as the number of rows of _**B**_.

## Organization of This Book

Chapter 2 introduces the basic terminology and concepts behind statistical learning. 
This chapter also presents the *K-nearest neighbor* classifier, a very simple method that works surprisingly well on many problems. 
Chapters 3 and 4 cover classical linear methods for regression and classification.
In particular, Chapter 3 reviews *linear regression*, the fundamental starting point for all regression methods. 
In Chapter 4 we discuss two of the most important classical classification methods, *logistic regression* and *linear discriminant analysis*.

A central problem in all statistical learning situations involves choosing the best method for a given application. 
Hence, in Chapter 5 we introduce *cross-validation* and the *bootstrap*, which can be used to estimate the accuracy of a number of different methods in order to choose the best one.

Much of the recent research in statistical learning has concentrated on non-linear methods. 
However, linear methods often have advantages over their non-linear competitors in terms of interpretability and sometimes also accuracy. 
Hence, in Chapter 6 we consider a host of linear methods, both classical and more modern, which offer potential improvements over standard linear regression. 
These include *stepwise selection*, *ridge regression*, *principal components regression*, *partial least squares*,and the *lasso.*

The remaining chapters move into the world of non-linear statistical learning. 
We first introduce in Chapter 7 a number of non-linear methods that work well for problems with a single input variable. 
We then show how these methods can be used to fit non-linear *additive* models for which there is more than one input. 
In Chapter 8, we investigate *tree-based* methods, including *bagging*, *boosting*,and *random forests*. 
*Support vector machines*, a set of approaches for performing both linear and non-linear classification, are discussed in Chapter 9.
Finally, in Chapter 10, we consider a setting in which we have input variables but no output variable. 
In particular, we present *principal components analysis*, *K-means clustering*,and *hierarchical clustering*.

At the end of each chapter, we present one or more `R` lab sections in which we systematically work through applications of the various methods discussed in that chapter. 
These labs demonstrate the strengths and weaknesses of the various approaches, and also provide a useful reference for the syntax required to implement the various methods. 
The reader may choose to work through the labs at his or her own pace, or the labs may be the focus of group sessions as part of a classroom environment.
Within each `R` lab, we present the results that we obtained when we performed the lab at the time of writing this book. 
However, new versions of `R` are continuously released, and over time, the packages called in the labs will be updated. 
Therefore, in the future, it is possible that the results shown in
the lab sections may no longer correspond precisely to the results obtained by the reader who performs the labs. 
As necessary, we will post updates to the labs on the book website.

We use the UPHILL symbol to denote sections or exercises that contain more challenging concepts. 
These can be easily skipped by readers who do not wish to delve as deeply into the material, or who lack the mathematical background.

## Data Sets Used in Labs and Exercises

In this textbook, we illustrate statistical learning methods using applications from marketing, finance, biology, and other areas. 
The `ISLR` package available on the book website contains a number of data sets that are required in order to perform the labs and exercises associated with this book. 
One other data set is contained in the `MASS` library, and yet another is part of the base `R` distribution. 
Table 1.1 contains a summary of the data sets required to perform the labs and exercises. 
A couple of these data sets are also available as text files on the book website, for use in Chapter 2.


```r
# Make the dataframe of datasets
tribble(
  ~Name, ~Description,
  "Auto", "Gas mileage, horsepower, and other information for cars.",
  "Boston", "Housing values and other information about Boston suburbs.",
  "Caravan", "Information about individuals offered caravan insurance.",
  "Carseats", "Information about car seat sales in 400 stores.",
  "College", "Demographic characteristics, tuition, and more for USA colleges.",
  "Default", "Customer default records for a credit card company.",
  "Hitters", "Records and salaries for baseball players.",
  "Khan", "Gene expression measurements for four cancer types.",
  "NCI60", "Gene expression measurements for 64 cancer cell lines",
  "OJ", "Sales information for Citrus Hill and Minute Maid orange juice.",
  "Portfolio", "Past values of financial assets, for use in portfolio allocation.",
  "Smarket", "Daily percentage returns for S&P 500 over a 5-year period.",
  "USArrests", "Crime statistics per 100,000 residents in 50 states of USA.",
  "Wage", "Income survey data for males in central Atlantic region of USA.", 
  "Weekly", "1,089 weekly stock market returns for 21 years."
) %>% 
  # use reactable to produce the table
  reactable(
    borderless = T, 
    defaultPageSize = 15,
    sortable = F,
    # Specific column formats
    columns = list(
      Name = colDef(
        width = 120,
        style = list(color = "#B44C1C",
                     fontFamily = "Roboto Mono"))
    )
  )

# Put the caption beneath the table
rt_caption(
  text = str_c("A List of the data sets to perform the labs and exercises in this textbook. All data sets are available in the ", data_colour("ISLR"), " library, with the exception of ", data_colour("Boston"), " (part of  ", data_colour("MASS"), ") and ", data_colour("USArrests"), " (part of the base ", data_colour("R"), " distribution)."), 
  tab_num = 1.1
  )
```


<!--html_preserve--><div id="htmlwidget-db2502520cc23df2a254" class="reactable html-widget" style="width:auto;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-db2502520cc23df2a254">{"x":{"tag":{"name":"Reactable","attribs":{"data":{"Name":["Auto","Boston","Caravan","Carseats","College","Default","Hitters","Khan","NCI60","OJ","Portfolio","Smarket","USArrests","Wage","Weekly"],"Description":["Gas mileage, horsepower, and other information for cars.","Housing values and other information about Boston suburbs.","Information about individuals offered caravan insurance.","Information about car seat sales in 400 stores.","Demographic characteristics, tuition, and more for USA colleges.","Customer default records for a credit card company.","Records and salaries for baseball players.","Gene expression measurements for four cancer types.","Gene expression measurements for 64 cancer cell lines","Sales information for Citrus Hill and Minute Maid orange juice.","Past values of financial assets, for use in portfolio allocation.","Daily percentage returns for S&P 500 over a 5-year period.","Crime statistics per 100,000 residents in 50 states of USA.","Income survey data for males in central Atlantic region of USA.","1,089 weekly stock market returns for 21 years."]},"columns":[{"accessor":"Name","name":"Name","type":"character","width":120,"style":{"color":"#B44C1C","fontFamily":"Roboto Mono"}},{"accessor":"Description","name":"Description","type":"character"}],"sortable":false,"defaultPageSize":15,"paginationType":"numbers","showPageInfo":true,"minRows":1,"borderless":true,"theme":{"color":"#333","tableStyle":{"borderTop":"2px solid black","borderBottom":"2px solid black"},"headerStyle":{"borderBottom":"2px solid black"}},"dataKey":"f03e1d6d81a6e92d19bbeef08f5de63f","key":"f03e1d6d81a6e92d19bbeef08f5de63f"},"children":[]},"class":"reactR_markup"},"evals":[],"jsHooks":[]}</script><!--/html_preserve--><!--html_preserve--><p class="caption">
<span class="tab-num">TABLE 1.1.</span>
<em>A List of the data sets to perform the labs and exercises in this textbook. All data sets are available in the <strong><span style='font-family:monospace; color: #B44C1C;'>ISLR</span></strong> library, with the exception of <strong><span style='font-family:monospace; color: #B44C1C;'>Boston</span></strong> (part of  <strong><span style='font-family:monospace; color: #B44C1C;'>MASS</span></strong>) and <strong><span style='font-family:monospace; color: #B44C1C;'>USArrests</span></strong> (part of the base <strong><span style='font-family:monospace; color: #B44C1C;'>R</span></strong> distribution).</em>
</p><!--/html_preserve-->

It contains a number of resources, including the `R` package associated with this book, and some additional data sets.

## Book Website

The website for this book is located at

[www.StatLearning.com](www.StatLearning.com)

## Acknowledgements

A few of the plots in this book were taken from ESL: Figures 6.7, 8.3, and 10.12. 
All other plots are new to this book.
