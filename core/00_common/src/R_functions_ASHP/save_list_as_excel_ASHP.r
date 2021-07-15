# define  : 


# list to save			<- signatures_to_test
# path					<- "data/processed/"

# Output: data/processed/signatures_to_test.xlsx

	save.excel <-function(.list, default = 'var1', path = ''){
					require("XLConnect")
					.name <- as.list(match.call())[2]
					if(is.language(.name[[1]])) wb_name <- paste0(paste0(path, default, collapse = '/'), '.xlsx')
					if(is.symbol(.name[[1]])) wb_name <- paste0(paste0(path, as.character(.name), collapse = '/'), '.xlsx')
					wb <- loadWorkbook(wb_name, create = TRUE)
					createSheet(wb, names(.list))
					writeWorksheet(wb,.list, names(.list),header=FALSE)
					saveWorkbook(wb)
					}