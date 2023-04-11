
options(stringsAsFactors=F)
suppressMessages(library(tidyverse))
suppressMessages(library(janitor))
if(interactive()) {
  setwd('~/d/sci/src/mok_commentary')
}

dat = read_tsv('data/prodromes.tsv', col_types=cols())


resx=300
png('display_items/figure-1.png',width=3.25*resx,height=3.25*resx,res=resx)
par(mar=c(3,3,1,1))

ats = rep(1:9, 3) * 10^rep(-1:1, each=9)
bigs = c(1, 10, 100)
xlims = c(0.3, 50)
ylims = c(0.3, 50)
plot(NA, NA, xlim=xlims, ylim=ylims, xaxs='i', yaxs='i', axes=F, ann=F, log='xy')

# xlims = c(0,25)
# ylims = c(0,16)
# plot(NA, NA, xlim=xlims, ylim=ylims, xaxs='i', yaxs='i', axes=F, ann=F)
# ats = 0:25
# bigs = c(0,10,20)
axis(side=1, at=ats, tck=-0.025, labels=NA)
axis(side=1, at=bigs, tck=-0.05, labels=NA)
axis(side=1, at=bigs, lwd=0, line=-0.5)
mtext(side=1, line=1.5, text='NfL prodrome (years)')
axis(side=2, at=ats, tck=-0.025, labels=NA)
axis(side=2, at=bigs, tck=-0.05, labels=NA)
axis(side=2, at=bigs, lwd=0, line=-0.25, las=2)
mtext(side=2, line=1.75, text='disease duration (years)')
points(dat$nfl_prodrome, dat$dz_duration, pch=19)
# dat$ylab = case_when(dat$gene=='SOD1' ~ dat$dz_duration+0.5,
#                      dat$gene=='HTT' ~ dat$dz_duration+1,
#                      TRUE ~ dat$dz_duration)
# dat$xlab = case_when(dat$gene=='HTT' ~ dat$nfl_prodrome+1,
#                      TRUE ~ dat$nfl_prodrome)

dat$ylab = case_when(dat$gene=='HTT' ~ dat$dz_duration+4,
                     dat$gene=='PSEN1' ~ dat$dz_duration-2,
                     TRUE ~ dat$dz_duration)
dat$xlab = case_when(dat$gene=='HTT' ~ dat$nfl_prodrome+3,
                     TRUE ~ dat$nfl_prodrome)
dat$genepad = ifelse(dat$pos==2, paste0(gsub('[A-Z0-9±~]',' ',dat$mut),' '), '')
dat$mutpad = ifelse(dat$pos==4, paste0(gsub('[A-Z0-9±~]',' ',dat$gene),' '), '')
par(xpd=T)
text(x=dat$xlab, y=dat$ylab, labels=paste0(dat$gene, dat$genepad), pos=dat$pos, font=3, family='mono')
text(x=dat$xlab, y=dat$ylab, labels=paste0(dat$mutpad, dat$mut), pos=dat$pos, font=1, family='mono')
par(xpd=F)
dev.off()

# summary(lm(dz_duration ~ nfl_prodrome, data=dat))
# summary(lm(dz_duration ~ log(nfl_prodrome), data=dat))

# x = 0:250/10
# y = (2.0597+3.4659*log(x))
# points(x,y,col='red',lty=3,type='l')