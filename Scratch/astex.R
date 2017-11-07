e <- quo(a + b + c + d)

fcn <- e[[1]]
arg1 <- e[[2]]
arg2 <- e[[3]]


dplyr::select.tbl_df


a <- 10
b <- 5
c <- 3
d <- 2
# this is the same function as in the previous example
f1 <- function(expr) {
	rlang::enquo(expr)
}
# make the quosure
q1 <- f1(a-b-c-d)
# evaluate the quosure, that is compute the value of a-b-c-d
rlang::eval_tidy(q1)

sum(c(1,2))


t1 <- tibble::tribble(
	~col1, ~col2, ~col3,
	1, 2, 3,
	4, 5, 6,
	7, 8, 9
)
dplyr::select(t1, col2:col3)
rlang::type_of(t1)
attributes(t1)

t2 <- subset(t1, select=c("col2", "col3"))
colnames(t2) <- c("first", "last")
t2
tibble::tibble(t1[["col2"]], t1[["col3"]])

t1 <- data.frame(
	col1 = 1:3,
	col2 = 2:4,
	col3 = 3:5
)
t2 <- subset(t1, T, select = c(col2, col3))
colnames(t2) <- c("first", "last")
t2

subset(airquality, Temp > 80, select = c(Ozone, Temp))



t1 <- tibble::tribble(
	~col1, ~col2, ~col3,
	1:3,
	2:4,
	3:5
)
# you just specify the columns you want with literal names
dplyr::select(t1, first=col2, last=col3)

iris <- dplyr::as_tibble(iris) # so it prints a little nicer
dplyr::select(iris, dplyr::starts_with("Petal"))
select(iris, ends_with("Width"))

df <- as.data.frame(matrix(runif(100), nrow = 10))
df <- tbl_df(df[c(3, 4, 7, 1, 9, 8, 5, 2, 6, 10)])
dplyr::select(df, V4:V6)
select(df, num_range("V", 4:6))




t1[,c("col1", "col2")]

# function finds the type of a quosure 
is_quo_tests <- tibble::tribble(
		
		~test, ~ title,
		rlang::quo_is_lang, t = "quo_lang",
		rlang::quo_is_null, "quo_null",
		rlang::quo_is_symbol,"quo_symbol",
    rlang::quo_is_symbolic, "quo_symbolic",
		rlang::quo_is_missing, "quo_missing")

what_kind_of_quosure <- function(obj) {
	is_quo_tests$title %>%
		tibble::tibble() %>%
		dplyr::filter(purrr::map_lgl(is_quo_tests$test, ~ .(obj))) %>%
		purrr::flatten_chr()
}


what_kind_of_quosure(e)

te_what_is_it(e1)
te_what_is_it(e2)

what_kind_of_quosure(e1)
what_kind_of_quosure(e2)

e1 <- arg1[[1]]
e2 <- arg1[[2]]


q9 <- quo(zzz)
typeof(q9)
methods::getClass(typeof(q9))
te_what_is_it(q9)
is.call(q9)

# doesn't expand
e3 <- quo(e2)

e4 <-expr_interp(e2)

e4[1]
e4_1 <- e4[[2]]
e4_2 <- e4[[3]]

what_kind_of_quosure(e4_1)

te_what_is_it(e4_2)

e4_1_1[[2]]

te_what_is_it(e4_1)


e4_1_1  <- expr_interp(e4_1)

e4_1_1[[2]]

what_kind_of_quosure(e4_1)
what_kind_of_quosure(e4_1_1)

eval_tidy(qq)

qq1 <- quo(qq)
what_kind_of_quosure(qq1[[1]])
qq1[[1]]
qq1[[2]]
te_what_is_it(qq1)
eval_tidy(qq)
a
a1 <- quo(a)
a2 <- as_quosure(a1)
what_kind_of_quosure(a1)
what_kind_of_quosure(a2)
attributes(a1)
attributes(a2)

a2 <- quote(a)
a3 <- as_quosure(a2)

is_quosure(a2)
is_quosure(a3)

# example of symbol, 'ccc' which has not been assigned a value
# and is 'quoted'd converted to a quosure with an
# environment that includes a value for "ccc'
a5 <- quote(ccc)
a_env <- new.env()
a_env$ccc = 14
a6 <- as_quosure(a5, a_env)
eval_tidy(a6)

# this shows and a quosure has an associated environment in
# attribute .Environment. In the global environment ggg is 19
# before ggg_q is evaluated the value of ggg in .Environment
# is changed to 30

ggg <- 19
ggg_q <- quo(ggg)
eval_tidy(ggg_q)
attr(ggg_q, ".Environment")$ggg
attr(ggg_q, ".Environment")$ggg <- 30
eval_tidy(ggg_q)

# in this example hhh is an uninitialized value
# but its value is set in the .Environment attibute
# of the hhh_q quosure
hhh_q <- quo(hhh)
attr(hhh_q, ".Environment")$hhh
attr(hhh_q, ".Environment")$hhh <- 40
eval_tidy(hhh_q)

# this shows that an unevaluated value cannot
# be found in the .Environment associated with
# the quosure
lll_q <- quo(lll)
lll_e <- attr(lll_q, ".Environment")
l <- ls(lll_e)
l[l == "lll"]

a
b


ab1 <- 



eval_tidy(a1)
a

qq1[[2]]


e4_1_1[[1]]
e4_1_1[[2]]
z <- e4_1_1[[3]]
te_what_is_it(z)
te_what_is_it(e4_1_1)
what_kind_of_quosure(z)

b <- 88

eval_tidy(e4_1_1)


length(e4_1_1)

e3[[1]]

te_what_is_it(z)

z <- a + b + c + d

expr_interp(z)




other_fn <- function(x) toupper(x)


fn <- expr_interp(function(x) {
	x <- paste0(x, "_suffix")
	!!! body(other_fn)
})
fn
fn("foo")




te_what_is_it(z)
z[1]
quo_text(e)



expr_interp(a + b + c + d)

# function that builds a tabular ast of a quosure
make_initial_ast_table <- function() {
ast_table_env <- new.env
ast_table_env$ast <- tibble::tribble(~nodeid, ~rsibling, ~parentid, ~depth, ~quo)
ast_table_env
}

ls()

mm <- make_initial_ast_table()


build_ast <- function(quosure, ast_table_env = NA, parentid = NA) {
	id <- 1
	if(is.na(ast_table_env)) {
		ast_table_env <- new.env()
		ast_table_env$ast <- tibble::tribble(~nodeid, ~rsibling, ~parentid, ~depth, ~quo)
	}
	if(nrow(ast_table_env$ast) != 0) {
		id <- max(ast_table$ast[["id"]])
	}
	r <- tibble::tribble(~nodeid, ~rsibling, ~parentid, ~depth, ~quo,
											 id, NA, NA, 1, quosure)
	ast_table_env$ast <- dplyr::bind_rows(ast_table_env$ast, r)
	ast_table_env$ast
}

qf1 <- rlang::quo(a-b-c-d)

headqf1 <- rlang::lang_head(qf1)
tailqf1 <- rlang::lang_tail(qf1)


q3 <- rlang::expr_interp(tailqf1)[[1]]

q <- rlang::quo(a-b-c-d)
rlang::lang_tail(q)

f10 <- function(expr) {
	q <- rlang::enquo(expr)
	return(q)
}

f10(a-b-c-d)



rm("drill_down")



make_part_number <- function(expr) {
	# used for recursion
	eval_attrs <- function(q, part_attributes = vector(mode = "character")) {
		if(!rlang::is_lang(q)) {
			# 1 is not a language object so just evaluate it
			# and add it to the list of part attributes
			part_attributes <- c(part_attributes, as.character(rlang::eval_tidy(q)))
			# and finish the recursion
			return(part_attributes)
		}
		# if we gt to here q is a language object so it has a tail
		tail <- rlang::lang_tail(q)
		# add the second entry in the tail to the part attributes
		part_attributes <- 
			c(part_attributes, 
				as.character(rlang::eval_tidy(rlang::expr_interp(tail[[2]]))))
		# recurse to find the next part number attribute
		eval_attrs(rlang::expr_interp(tail[[1]]), part_attributes)
	}
	# make quosure out expr
	q <- rlang::enquo(expr)
	# recurse to find all the part number attributes
	atrs <- eval_attrs(q)
	# concatonate all the part number attributes with "-"
	stringr::str_c(atrs, collapse="-")	
}
a <- 10
b <- 9
c <- 4
d <- 7
make_part_number(a-b-c-d)

q3 <- rlang::expr_interp(tail2)

make_part_number2 <- function(expr) {
	q <- rlang::enquo(expr)
	text <- rlang::f_text(q)
	atrs <- stringr::str_trim(stringr::str_extract_all(text, "[^-]+"))
	atrs
}

make_part_number2(a-b-c-d)

rlang::lang_head(tail2)
rlang::lang_tail(tail2)



rlang::enquo

expr_interp(a-b-c-d)

r1 <- rlang::f_rhs(q10)
r2 <- rlang::f_lhs(q10)

te_what_is_it(r1)

rlang::body(q10)

rlang::lang_head




global_env <- environment()
a <- 10
b <- 5
c <- 3
d <- 2
f1 <- function() {
	q <- rlang::quo(a-b-c-d)
	q	
}
q1 <- f1()
q1env <- attr(q1, ".Environment")
pryr::address(global_env)
pryr::address(q1env)
ls(global_env)
ls(q1env)



q20 <- rlang::quo(a-b-c)
headq20 <- rlang::lang_head  (q20)
tailq20 <- rlang::lang_tail(q20)
a
rlang::eval_tidy(tailq20[[1]])

rlang::lang_head(tailq20[[1]])
rlang::lang_tail(tailq20[[2]])


rlang::f_lhs(tailq20)
rlang::lang_head(tailq20)

# head and tail
q10 <- rlang::quo(a - b - c -d)
headq10 <- rlang::lang_head  (q10)
tailq10 <- rlang::lang_tail(q10)
car <- rlang::node_car(q10)
cdr <- rlang::node_cdr(q10)
rlang::node_caar(q10)
rlang::node_cadr(q10)
rlang::node_cddr(q10)
rlang::node_tag(q10)

envq10 <- attr(q10, ".Environment")

pryr::address(envq10)
envg <- environment()

pryr::address(envg)


is.list(tailq10)
length(tailq10)


tailq10
q11 <- rlang::get_expr(tailq10[[1]])
headq11 <- rlang::lang_head (q11)
tailq11 <- rlang::lang_tail(q11)

getClass("binary_lang")

rlang::is_lang(q11)
te_what_is_it(q11)
rlang::get_expr(q11)
te_what_is_it(q11)


rlang::lang_head

# this gets the rhs which is the last element in q10, in effect, q[[2]] the expession
rhs <- rlang::f_rhs(q10)
lhs <- rlang::f_lhs(q10)

#this interprets q10[[2]] as quo does a literal expression
e10 <- expr_interp(r10)
head <- rlang::lang_head  (r10)
tail <- rlang::lang_tail(r10)
car <- rlang::node_car(r10)
cdr <- rlang::node_cdr(r10)
rlang::node_caar(r10)
rlang::node_cadr(r10)
rlang::node_cddr(r10)

is.list(tail)
length(tail)

te_what_is_it(tail[[1]])
eval_tidy(tail[[1]])
e10[1]
e10[2]
e10[3]
# this gets the last element in e10, which is the expression pruned off of q10
re1 <- rlang::f_rhs(e10)
#this gets the remaining expression after pruning
le1 <- rlang::f_lhs(e10)
# this makes a quosure out of le1
e2 <- rlang::expr_interp(le1)
e2[[1]]
e2[[2]]
e2[[3]]


is_formula(q10)
q10[1]
q10[2]

q11 <- rlang::expr_interp(q10[[2]])
q12 <- expr_interp(q11[[2]])
q10[[3]]
expr_interp

rlang:::rlang_interp


q11[2]
q12[2]
