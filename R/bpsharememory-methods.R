setMethod("bpsharememory", "ANY", function(x) FALSE)

setMethod("bpsharememory", "missing",
    function(x)
{
    x <- registered()[[1]]
    bpsharememory(x)
})

