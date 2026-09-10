# A metadata-only wrapper around the unchanged method implementation.
.rs_method_fit <- fit_response
fit_response <- function(counts,samples,config) {
 rs_stamp_result(.rs_method_fit(counts,samples,config))
}
