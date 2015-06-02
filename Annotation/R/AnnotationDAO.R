#'Provides a generic annotation data access object.
#'Allows for a low memory footprint and efficient data access.
#'
#'@name AnnotationDAO
#'@title AnnotationDAO
#'@field configurer - holds global configuration data in Configurer object
#'@field conn - a connection to a temporary database holding the annotation
#'@field table_name - the name of the table to put data in
#' @export AnnotationDAO
#' @exportClass AnnotationDAO
if(!(require("R6", quietly=T))) {
	install.packages("R6")
	library(R6, quiet=T)
}
AnnotationDAO <- R6Class("AnnotationDAO",
	public = list(
	   configurer = NULL,
	   conn = NULL,
	   table_name = NULL,
	   
	   initialize =  function(configurer, annotation) {
		   #Create a new DAO object with AnnotationDAO$new(configurer), not by
		   #using this method.
		   #>annotation -- data.frame with annotation
		   #<returns nothing
		   
		   if(!(require("RSQLite", quietly=T))) {
			   install.packages("RSQLite")
			   library(RSQLite, quiet=T)
		   }
		   
		   self$configurer <- configurer
		   self$table_name <- 'temp_table'
		   
		   self$load_annotation_data(annotation)
		   
		   reg.finalizer(self,
				   function(e) dbDisconnect(self$conn),
				   onexit = TRUE)
	   }, #end initialize
	   
	   load_annotation_data = function(annotation) {
		   # Given a filename with annotation data, load it into
		   # a temporary database. The annotation should have
		   # row names in first column, column names in first row,
		   # and be tab-separated.
		   #
		   #>annotation -- data.frame with annotation data
		   #<returns nothing
		   
		   # open a connection to a temporary database
		   self$conn <- dbConnect(RSQLite::SQLite(), dbname="")
		   
		   dbWriteTable(self$conn, self$table_name, annotation, row.names=1, header=T, sep='\t')
	   }, #end load_annotation_data
	   
	   query_full_annotation = function() {
		   # returns full annotation in the database as a data.frame
		   
		   return(dbReadTable(self$conn, self$table_name))
	   }, #end get_full_annotation
	   
	   query_columns = function(columns, constraints=NULL) {
		   # Frequently, the full annotation won't be needed. Just get the
		   # columns passed.
		   #
		   # >columns -- a vector of column names to retrieve
		   # >contraints -- such as MAPINFO > 1000000 and CHR = 7
		   # <returns the contents of the columns asked for
		   if(length(constraints) > 0) {
			   where_clause <- paste0(' where ', constraints)
		   } else {
			   where_clause <- ''
		   }
		   
		  	col_str <- paste(columns, collapse=", ")
			subtable <- dbGetQuery(self$conn, paste0('select ', col_str, ' from ', self$table_name, 
							   where_clause))
		   return(subtable)
	   }, #end get_columns
	   
	   query_column_names = function() {
		   colvector <- dbListFields(self$conn, self$table_name)
		   return(colvector)
	   } #end get_column_names	   
	) # end public
) #end AnnotationDAO