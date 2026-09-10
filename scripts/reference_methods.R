# Strong baseline: separately implemented IUT claims. This deliberately shares
# preprocessing/model inputs, and is an implementation check, not a new method.
reference_claims <- function(results, config) {
  z <- results[is.finite(results$d) & is.finite(results$t) & is.finite(results$r), , drop=FALSE]
  up <- function(x, bound, se, df) pt((bound-x)/se, df=df)
  pieces <- list()
  for (s in c(1,-1)) {
    pd <- up(s*z$d,config$delta_D,z$d_se,z$df)
    improve <- up(-s*z$t,config$delta_M,z$t_se,z$df)
    ps <- list(
      within_reference=pmax(pd,improve,up(z$r,-config$epsilon_R,z$r_se,z$df),up(-z$r,-config$epsilon_R,z$r_se,z$df)),
      partial_reversal=pmax(pd,improve,up(s*z$r,config$epsilon_R,z$r_se,z$df)),
      overshoot=pmax(pd,improve,up(-s*z$r,config$epsilon_R,z$r_se,z$df)),
      further_deviation=pmax(pd,up(s*z$t,config$delta_M,z$t_se,z$df)),
      no_meaningful_change=pmax(pd,up(z$t,-config$epsilon_0,z$t_se,z$df),up(-z$t,-config$epsilon_0,z$t_se,z$df)))
    for (cl in names(ps)) pieces[[length(pieces)+1L]] <- data.frame(gene_id=z$gene_id,intervention=z$intervention,class_name=cl,direction=s,p_raw=ps[[cl]],stringsAsFactors=FALSE)
  }
  a <- do.call(rbind,pieces)
  a$adjusted_p_BY <- p.adjust(a$p_raw,'BY')
  a$adjusted_p_BH <- p.adjust(a$p_raw,'BH')
  a$raw_p <- a$p_raw; a$adjusted_p <- a$adjusted_p_BY
  a$rejected <- a$adjusted_p_BY<=config$alpha
  a$rejected_BH <- a$adjusted_p_BH<=config$alpha
  a
}

# Independent per-gene weighted QR. Does not read the kernel's final standard
# errors or variances. Reuses limma empirical Bayes squeezing (explicitly allowed).
reference_qr_fit <- function(E,W,X) {
  n <- nrow(E); p <- ncol(X)
  beta <- matrix(NA_real_,n,p,dimnames=list(rownames(E),colnames(X)))
  covariance <- vector('list',n); s2 <- numeric(n); df <- rep(nrow(X)-p,n)
  for(i in seq_len(n)) {
    a <- X*sqrt(W[i,]); b <- E[i,]*sqrt(W[i,])
    q <- qr(a,LAPACK=FALSE,tol=1e-12)
    if(q$rank!=p) stop('Reference weighted QR is rank deficient')
    beta[i,] <- qr.coef(q,b)
    s2[i] <- sum(qr.resid(q,b)^2)/df[i]
    Ri <- backsolve(qr.R(q),diag(p))
    v <- tcrossprod(Ri)
    covariance[[i]] <- v[order(q$pivot),order(q$pivot),drop=FALSE]
  }
  eb <- limma::squeezeVar(s2,df,covariate=NULL,robust=FALSE)
  list(coefficients=beta,covariance=covariance,residual_variance=s2,
       posterior_variance=eb$var.post,df_total=pmin(df+eb$df.prior,sum(df)),df_prior=eb$df.prior,s2_prior=eb$var.prior)
}
independent_qr_fit <- reference_qr_fit
