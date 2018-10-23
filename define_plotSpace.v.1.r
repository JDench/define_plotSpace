# This function takes a defined amount of space nad divides it into an equal number of sub-spaces based on the number of sub-groups defined
# as well as the amount of space between final groupings.  The subDivs should be a matrix with the sets of sequentially nested subdivisions 
# we want to define space for the tmpScale is the total area which can be allocated and the separator between final units of space 
# This function will return a list of lists for the ranges at each level.  This function resolves lists by sequentially calling itself through all list items
# The tmpSpace is used to define what range of values can be permitted, tmpBuffer is used to define the proportion of given space to use as trim/ or not use....
# The last argument is to querry if we want to order the values being subdivided.
define_plotSpace <- function(func_subDivs, func_tmpSpace = c(0,1),func_tmpBuffer = 0.1, func_tmpBuffer_max = 0.4, func_tmpBuffer_min=0.2, 
							func_orderDivs = FALSE, func_pairsBeside = TRUE){
	# We define what are the divisions of space that we want to pass to each part, we add one so that this forces the division points to be boundaries
	# between our spaces to be defined for the current func_tmpSpace
	func_tmpDivs <- if(is.matrix(func_subDivs) || is.data.frame(func_subDivs)){
						seq(func_tmpSpace[1], func_tmpSpace[2],length.out = length(unique(func_subDivs[,1]))+1)
					} else {
						seq(func_tmpSpace[1], func_tmpSpace[2],length.out = length(unique(func_subDivs))+1)
					}
	# Now to define the actual space permissable we create space sets for all but the first and last elements, which are the boundary points.
	func_tmpDivs <- lapply(1:(length(func_tmpDivs)-1),function(func_thisSpace){ 
								# We find the space that could be occupied
								func_tmpSpace <- c(func_tmpDivs[func_thisSpace],func_tmpDivs[func_thisSpace+1])
								# So the actual space we'll return as permissible will be these boundaries, with trim of total space
								# BUT, if we're supposed to not have paired groups split apart, then we adjust how this value is calculated
								if (!is.matrix(func_subDivs) && !is.data.frame(func_subDivs) && func_pairsBeside){
									# Then the only values that get trimmed are the first and last
									if(func_thisSpace == 1){
										c(func_tmpSpace[1] + max(func_tmpBuffer_min, min(func_tmpBuffer_max,(func_tmpBuffer * (func_tmpSpace[2]-func_tmpSpace[1])))), 
												func_tmpSpace[2])
									} else if(func_thisSpace == (length(func_tmpDivs)-1)){
										c(func_tmpSpace[1],
											func_tmpSpace[2] - max(func_tmpBuffer_min, min(func_tmpBuffer_max,(func_tmpBuffer * (func_tmpSpace[2]-func_tmpSpace[1])))))
									} else {
										func_tmpSpace
									}
								} else {
									return( c(func_tmpSpace[1] + max(func_tmpBuffer_min, min(func_tmpBuffer_max,(func_tmpBuffer * (func_tmpSpace[2]-func_tmpSpace[1])))), 
												func_tmpSpace[2] - max(func_tmpBuffer_min, min(func_tmpBuffer_max,(func_tmpBuffer * (func_tmpSpace[2]-func_tmpSpace[1]))))) )
								}			
							})
	names(func_tmpDivs) <- if(is.matrix(func_subDivs) || is.data.frame(func_subDivs)){
								as.character(unique(func_subDivs[,1]))
							} else {
								as.character(unique(func_subDivs))
							}
	# Using the scale of space we've been given, we'll divide it by the nunber of unique instances in the first
	# column of func_subDivs (or if it's a vector simply by that value alone)
	if(is.matrix(func_subDivs) || is.data.frame(func_subDivs)){
		# If we've been asked to order our subDivs elements, we do so
		if(func_orderDivs){ func_subDivs <- func_subDivs[order(func_subDivs[,1]),] }
		# We also define what are the rows of the func_subDivs to be passed in each sub division
		func_tmpParts <- lapply(unique(func_subDivs[,1]),function(func_thisPart){ which(func_subDivs[,1] == func_thisPart) })
		# Now the we've defined what divisions of space and parts/rows of the current data need to be subdivided we iteratively call this forward.
		func_tmpReturn <- lapply(1:length(unique(func_subDivs[,1])),function(func_thisDiv){ 
									define_plotSpace(func_subDivs = func_subDivs[func_tmpParts[[func_thisDiv]],-1], 
													func_tmpSpace  = func_tmpDivs[[func_thisDiv]],
													func_tmpBuffer = func_tmpBuffer,
													func_tmpBuffer_max = func_tmpBuffer_max,
													func_tmpBuffer_min = func_tmpBuffer_min,
													func_orderDivs = func_orderDivs)
								})
		names(func_tmpReturn) <- names(func_tmpDivs)
		return( func_tmpReturn )
	} else {
		# This mean's we're on the last element of the divisions and we can return the divisions that we've defined
		return( func_tmpDivs )
	}
}
