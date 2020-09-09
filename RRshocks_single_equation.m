%% Dissertation
% Implication of RR shocks on labor market variables, 1979-2008
% Pritha Chaudhuri, April 2019

clear
clc
close all

data=xlsread('data/RomerandRomerDataAppendix.xls','DATA BY MEETING');
nobs=size(data,1);
mtgdate=data(:, 1);
dtarg=data(:,2);
oldtarg=data(:,3);
inflm=data(:,4);
infl0=data(:,5);
infl1=data(:,6);
infl2=data(:,7);
dinflm=data(:,8);
dinfl0=data(:,9);
dinfl1=data(:,10);
dinfl2=data(:,11);
gdpm=data(:,12);
gdp0=data(:,13);
gdp1=data(:,14);
gdp2=data(:,15);
dgdpm=data(:,16);
dgdp0=data(:,17);
dgdp1=data(:,18);
dgdp2=data(:,19);
u0=data(:,20);

clear data

% estimate monetary policy shocks
y_temp=dtarg;
X_temp=[ones(nobs,1) oldtarg gdpm gdp0 gdp1 gdp2 dgdpm dgdp0 dgdp1 dgdp2 ...
    inflm infl0 infl1 infl2 dinflm dinfl0 dinfl1 dinfl2 u0];

cond=sum(isnan(X_temp),2);
cond2=cond==0;
missing=find(cond2==0);

y_shocks=y_temp(cond2);
X_shocks=X_temp(cond2,:);

betahat=(X_shocks'*X_shocks)\X_shocks'*y_shocks;
ehat=y_shocks-X_shocks*betahat;
RSS=ehat'*ehat;
TSS=(y_shocks-ones(length(y_shocks),1)*mean(y_shocks))'*(y_shocks-ones(length(y_shocks),1)*mean(y_shocks));
R2=1-RSS/TSS;
sigmahat2=RSS/(length(y_shocks)-19);
sigmahat=sqrt(sigmahat2);
varcov=sigmahat2*(X_shocks'*X_shocks)^(-1); 
stderr=sqrt(diag(varcov));

resid=ehat;
for i=1:length(missing)
    c=missing(i);
    resid=[resid(1:c-1); NaN; resid(c:length(resid))];
end

% covert shocks to monthly series
year=zeros(nobs,1);
month=zeros(nobs,1);
count=1;

for i=1:nobs
    assert(numel(num2str(mtgdate(i))) <= 6);
    assert(numel(num2str(mtgdate(i))) >= 5);
    
    temp=num2str(mtgdate(i));
    
    if count<=296
        year(i)=str2double(temp(end-1:end))+1900;
        count=count+1;
    elseif count>296
        year(i)=str2double(temp(end-1:end))+2000;
        count=count+1;
    end
    
    
    if numel(num2str(mtgdate(i))) == 5
        month(i)=str2double(temp(1));
    elseif numel(num2str(mtgdate(i))) == 6
        month(i)=str2double(temp(1:2));
    end
end

year2=unique(year);
month2=unique(month);
nshocks=length(year2)*12;

cond_m=zeros(length(month),1);
for j=2:length(month)-1
            if month(j) == month(j+1)
                cond_m(j)=1;
            elseif month(j-1) == month(j)
                cond_m(j)=1;
            end
end


resid_monthly=zeros(length(year2),12);
for yy=1:length(year2)
    for mm=1:12
        for j=1:length(resid)
            
            if year(j)==year2(yy) && month(j)==mm
                resid_monthly(yy,mm)=resid(j);
            end
        end
        
        repeat=find(cond_m==1);
        for k=1:2:length(repeat)
            index=repeat(k);
            repeat_month=month(index);
            repeat_year=year(index);
            if repeat_year==year2(yy) && repeat_month==mm
                resid_monthly(yy,mm)=resid(index)+resid(index+1);
            end
        end
        
    end
end

resid_monthly(isnan(resid_monthly))=0;
% save mpshocksmonthly resid_monthly
save mpshocksmonthly6908 resid_monthly
resid_monthly=reshape(resid_monthly',[nshocks,1]);
resid_monthly_sum=cumsum(resid_monthly);

% data2=xlsread('data/RomerandRomerDataAppendix.xls','DATA BY MONTH');
% dff=data2(38:end,2);
% clear data2

% d1=datetime(1969,1,1);
% d2=datetime(2008,12,1);
% dd=(d1:calmonths(1):d2)';

% figure
% h1=plot(dd,resid_monthly,'b','LineWidth',1)
% dateFormat=28;
% datetick('x',dateFormat,'keeplimits','keepticks')
% % axis tight
% hbands=recessionplot
% set(hbands,'FaceAlpha',0.3)
% [ax,h2]=suplabel('Months')
% [ax,h3]=suplabel('Percent','y')
% set(h2, 'fontsize', 25, 'FontName', 'TimesNewRoman')
% set(h3, 'fontsize', 25, 'FontName', 'TimesNewRoman')
% set(gcf, 'PaperOrientation', 'landscape')
% print('-bestfit', '-dpdf', ['MPShockSeries.pdf'])

% resid_quarterly=sum(reshape(resid_monthly,3,[]));
% d1=datetime(1969,1,1);
% d2=datetime(2008,12,1);
% dd=d1:calmonths(3):d2;
% 
% figure
% h1=plot(dd,resid_quarterly,'b','LineWidth',1.25)
% hold on
% h4=plot(dd,zeros(length(resid_quarterly),1),'k--','LineWidth',0.5)
% hold off
% dateFormat=28;
% datetick('x',dateFormat,'keeplimits','keepticks')
% % axis tight
% hbands=recessionplot
% set(hbands,'FaceAlpha',0.3)
% set(gca, 'fontsize', 16)
% [ax,h2]=suplabel('Quarters')
% [ax,h3]=suplabel('Percent','y')
% set(h2, 'fontsize', 25, 'FontName', 'TimesNewRoman')
% set(h3, 'fontsize', 25, 'FontName', 'TimesNewRoman')
% set(gcf, 'PaperOrientation', 'landscape')
% print('-bestfit', '-dpdf', ['MPShockSeries-Q.pdf'])


%% regressions
% load data, 
% cpsmonthly=csvread('data/cpsmonthly7908.csv');
% hsu=cpsmonthly7908(:,17);
% lsu=cpsmonthly7908(:,18);
% hsloghours=cpsmonthly7908(:,21);
% lsloghours=cpsmonthly7908(:,22);
% rws=cpsmonthly7908(:,23);
% rwu=cpsmonthly7908(:,24);
% 
% clear cpsmonthly7908

% uH=data(:,2);
% uL=data(:,3);
% % hoursH=rdata(:,8);
% % hoursL=rdata(:,9);
% rwH=data(:,4);
% rwL=data(:,5);
% logip=data(:,6);
% 
% save regdata uH uL rwH rwL logip 
load regdata

dlogip=zeros(length(logip),1);
for i=2:length(dlogip)
    dlogip(i)=logip(i)-logip(i-1);
end

datat=[1979:1/12:2009-1/12];
samplestart=1979;
sampleend=2009-1/12;
samplet=[samplestart:1/12:sampleend];
depvar=uL( find(datat==samplestart) : find(datat==sampleend));
% depvar2=uL( find(datat==samplestart) : find(datat==sampleend));
shockst=[year(1):1/12:year(end)+1-1/12];
shocks=resid_monthly( find(shockst==samplestart) : find(shockst==sampleend));

nirf=48;
nlags_depvar=24;
nlags_mpshock=36;
T=length(samplet);
y=depvar;

% REGRESSORS
% adding shocks
lags_mpshock=zeros(T,nlags_mpshock);
for t=1:T
    for nlags=1:nlags_mpshock
        if t-nlags>0
            lags_mpshock(t,nlags)=shocks(t-nlags);
        end
    end 
end

% adding lagged dependent variable
lags_depvar=zeros(T,nlags_depvar);
for t=1:T
    for nlags=1:nlags_depvar
        if t-nlags>0
            lags_depvar(t,nlags)=depvar(t-nlags);
        end
    end
end

% % adding lags of depvar2
% lags_depvar2=zeros(T,nlags_depvar);
% for t=1:T
%     for nlags=1:nlags_depvar
%         if t-nlags>0
%             lags_depvar2(t,nlags)=depvar2(t-nlags);
%         end
%     end
% end

% X=[lags_depvar, lags_depvar2, lags_mpshock];
X=[lags_depvar, lags_mpshock];

% adding constant
X=[X, ones(size(X,1),1)];

% adding monthly dummies
step1=[eye(11); zeros(1,11)];
step2=kron(ones(ceil(size(X,1)/4),1),step1);
step3=step2(1:size(X,1),:);
X=[X, step3];

% estimation
betahat_ols=(X'*X)\X'*y;
ehat_ols=y-X*betahat_ols;
RSS_ols=ehat_ols'*ehat_ols;
TSS_ols=(y-ones(length(y),1)*mean(y))'*(y-ones(length(y),1)*mean(y));
R2_ols=1-RSS_ols/TSS_ols;
sigmahat2_ols=RSS_ols/(length(y)-72);
sigmahat_ols=sqrt(sigmahat2_ols);
varcov_ols=sigmahat2_ols*(X'*X)^(-1);
stderr_ols=sqrt(diag(varcov_ols));

% impulse response
irf=compute_irf(betahat_ols,nlags_mpshock,nirf,nlags_depvar);

% error bands
ndraws=100;
irf_draws=zeros(nirf,ndraws);
beta_draws=mvnrnd(betahat_ols,varcov_ols,ndraws)';

for draws=1:ndraws
    beta_loop=beta_draws(:,draws);
    irf_temp=compute_irf(beta_loop,nlags_mpshock,nirf,nlags_depvar);
    irf_draws(:,draws)=irf_temp;
end

irf_95=zeros(nirf,2);
for t=1:nirf
    irf_95(t,1)=prctile(irf_draws(t,:),5);
    irf_95(t,2)=prctile(irf_draws(t,:),95);
end

figure
plot([1:1:nirf],irf,'b','Linewidth',1.5)
hold on
plot([1:1:nirf],irf_95(:,1),'k--','Linewidth',1)
hold on
plot([1:1:nirf],irf_95(:,2),'k--','Linewidth',1)
hold on 
plot([1:1:nirf],zeros(nirf,1),'r','Linewidth',0.5)
hold on
axis([1 nirf -Inf Inf])
set(gca, 'fontsize', 16)
[ax,h1]=suplabel('Months')
[ax,h2]=suplabel('Percent','y')
set(h1, 'fontsize', 25, 'FontName', 'Times New Roman')
set(h2, 'fontsize', 25, 'FontName', 'Times New Roman')
set(gcf, 'PaperOrientation', 'landscape')
print('-bestfit', '-dpdf', ['RRsingle-uL.pdf'])

