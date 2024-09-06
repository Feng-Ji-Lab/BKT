setGeneric("save", function(model, loc) {
    standardGeneric("save")
})

setMethod("save", "Model", function(model, loc) {
    saveRDS(object = model, file = loc)
})

setGeneric("load", function(model, loc) {
    standardGeneric("load")
})

setMethod("load", "Model", function(model, loc) {
    orig_model <- readRDS(loc)

    for (attr in slotNames(orig_model)) {
        slot(model, attr) <- slot(orig_model, attr)
    }

    return(model)
})
