function irf=compute_irf(betahat_ols,nlags_mpshock,nirf,nlags_depvar)

% coefficients on lagged dependent variable
b=betahat_ols(1:nlags_depvar);
% coefficients on mp shocks
c=[betahat_ols(nlags_depvar+1:nlags_depvar+nlags_mpshock); zeros(nirf-nlags_mpshock,1)];

response=zeros(nirf,1);
response_step=zeros(nlags_depvar+nirf,1);

for t=1:nirf
    response(t)=c(t)+b'*flipud(response_step(t:t+nlags_depvar-1));
    response_step(nlags_depvar+t)=response(t);
end

irf=zeros(nirf,1);
for t=1:nirf
    irf(t)=sum(response(1:t));
end

end