#'Provides an Annotation object.
#'This object will hold an annotation in a memory efficient manner.
#'Only the data that is needed will actually be loaded.
#'
#'@name Annotation
#'@title Annotation
#'@field configurer - holds global configuration data in Configurer object
#'@field dao - a reference to a DAO for expression data
#' @export Annotation
#' @exportClass Annotation
if(!(require("R6", quietly=T))) {
	install.packages("R6")
	library(R6, quiet=T)
}
Annotation <- R6Class("Annotation",
  public = list(
    configurer = NULL,
    dao = NULL,
	initialize =  function(configurer, annotation) {
		#Create a new Annotation object for holding expression data.
		#But should be done by calling Annotation$new()
		#>annotation -- data.frame with annotation data
		
		self$configurer <- configurer
		
		self$dao <- AnnotationDAO$new(self$configurer, annotation)
		
	}, #end initialize
	
	get_full = function() {
		'This function returns all the annotation data held in the object 
		as a data.frame.
				
		<returns a data.frame with the full annotation.
		'
		return(self$dao$query_full_annotation())
	}, #end get_full
	
	get_columns = function(columns, constraints = NULL) {
		'This method returns only the columns asked for from
		an annotation table.
				
		> columns -- a vector of column names to be returned
				
		> constraints -- optional string containing constraints to place
		on request (expert use only).
				
		< returns a data.frame with only the columns asked for of the
		annotation'
		return(self$dao$query_columns(columns, constraints))
	}, #end get_columns
	
	get_column_names = function() {
		'Returns all the column names in the annotation.'
		return(self$dao$query_column_names())
	}
  ) # end public
) #end Annotation