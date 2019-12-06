raw = read.csv('stats.csv')
ipc = read.csv('IPCResults.csv')
results = merge(x = ipc, y = raw, by = 'Problem', all = TRUE)
rm(ipc)
rm(raw)
vars = ncol(results)
library(dplyr) # filter
library(ggplot2)
library(ggrepel)
require(gridExtra)
require(cowplot)
a = 0.7
ps = 3
ts = 2
cutoff = 200000
tedious = filter(results, results$ms > cutoff)
planner = factor(tedious$Planner)
domain = factor(tedious$Dom)
shapes = 1:nlevels(domain)
order = tedious$n
size = tedious$m
runtime = tedious$ms
special = c(12, 16, 17, 20, 21, 23, 44, 52, 55, 58, 61)
for (s in special) {
    target = names(results)[s]
    p0 = ggplot(tedious, aes_string(x = order, y = size)) +
        geom_point(aes(color = planner, shape = domain)) +
        scale_shape_manual(values = shapes) +
        theme(legend.box = "horizontal") +
        guides(fill=guide_legend(ncol=4))
    p1 = ggplot(tedious, aes_string(x = order, y = target)) +
        geom_point(aes(color = planner, shape = domain), size = ps, alpha = a) +
        scale_shape_manual(values = shapes) +
        theme(legend.position = "none") +
        xlab("Order") +
        ylab(target) +
        geom_text_repel(tedious, mapping = aes(label = Problem), size = ts, hjust = 0, nudge_x = 5)
    p2 = ggplot(tedious, aes_string(x = size, y = target)) +
        geom_point(aes(color = planner, shape = domain), size = ps, alpha = a) +
        scale_shape_manual(values = shapes) +
        theme(legend.position = "none") +
        xlab("Size") +
        ylab(target) +
        geom_text_repel(tedious, mapping = aes(label = Problem), size = ts, hjust = 0, nudge_x = 5)
    p3 = ggplot(tedious, aes_string(x = runtime, y = target)) +
        geom_point(aes(color = planner, shape = domain), size = ps, alpha = a) +
        scale_shape_manual(values = shapes) +
        theme(legend.position = "none") +
        xlab("Runtime") +
        ylab(target) +
        geom_text_repel(tedious, mapping = aes(label = Problem), size = ts, hjust = 0, nudge_x = 5)
    legend = cowplot::get_legend(p0)
    png(sprintf("s_nmt_%d.png", s), width = 1200, height = 1200)
    grid.arrange(p1, p2, p3, legend, top = target)
    dev.off()
}
quit()
planner = factor(results$Planner)
domain = factor(results$Dom)
shapes = 1:nlevels(domain)
order = results$n
size = results$m
runtime = results$ms
i = 1
for (target in names(results)) {
    p0 = ggplot(results, aes_string(x = order, y = size)) +
        geom_point(aes(color = planner, shape = domain)) +
        scale_shape_manual(values = shapes) +
        theme(legend.box = "horizontal") +
        guides(fill=guide_legend(ncol=4))
    p1 = ggplot(results, aes_string(x = order, y = target)) +
        geom_point(aes(color = planner, shape = domain), size = s, alpha = a) +
        scale_shape_manual(values = shapes) +
        theme(legend.position = "none") +
        xlab("Order") +
        ylab(target)
    p2 = ggplot(results, aes_string(x = size, y = target)) +
        geom_point(aes(color = planner, shape = domain), size = s, alpha = a) +
        scale_shape_manual(values = shapes) +
        theme(legend.position = "none") +
        xlab("Size") +
        ylab(target)
    p3 = ggplot(results, aes_string(x = runtime, y = target)) +
        geom_point(aes(color = planner, shape = domain), size = s, alpha = a) +
        scale_shape_manual(values = shapes) +
        theme(legend.position = "none") +
        xlab("Runtime") +
        ylab(target)
    legend = cowplot::get_legend(p0)
    png(sprintf("nmt_%d.png", i), width = 900, height = 900)
    i = i + 1
    grid.arrange(p1, p2, p3, legend, top = target)
    dev.off()
}

cc = read.csv('components.csv')
cc$stepcount = NULL # redundant
cc$ms = NULL
tmp = merge(x = results, y = cc, by = 'Problem', all = TRUE)
rm(tmp)
rm(cc)
layers = read.csv('layers.csv')
layers$stepcount = NULL # redundant
layers$ms = NULL
comb = merge(x = tmp2, y = layers, by = 'Problem', all = TRUE)
rm(tmp2)
rm(layers)
cat(names(comb)[1:6], '\n')
cat(sapply(comb, typeof)[1:6], '\n')
cat(sapply(comb, class)[1:6], '\n')
library("corrplot")
library("data.table")
factors = c(1, 2, 3, 6) # non-numeric measurements
mat = as.data.table(comb[, -factors]) # remove factors
w = 1300
h = 1300
m = mat[, .SD, .SDcols = names(mat) %like% "lvl"]
for (lvl in 0:3) {
    label = sprintf("lvl%d", lvl)
    cat(label, '\n')
    sel = mat[, .SD, .SDcols = names(mat) %like% label]
    m = sel[, .SD, .SDcols = names(sel) %like% "cc"]
    cm = cor(as.matrix(m), method = 'pearson', use = 'pairwise.complete.obs')
    filename = sprintf('corr_cc%s.png', label)
    cat(filename, '\n')
    png(filename, width = (5 - lvl) * w, height = (5 - lvl) * h)
    corrplot(cm, type = 'upper', tl.cex = 2)
    graphics.off()
    m = sel[, .SD, .SDcols = ! names(sel) %like% "cc"]
    cm = cor(as.matrix(m), method = 'pearson', use = 'pairwise.complete.obs')
    filename = sprintf('corr_%s.png', label)
    cat(filename, '\n')
    png(filename, width = w, height = h)
    corrplot(cm, type = 'upper', tl.cex = 2)
    graphics.off()

}
sel = mat[, .SD, .SDcols = names(mat) %like% "cc"]
m = sel[, .SD, .SDcols = ! names(sel) %like% "lvl"]
cm = cor(as.matrix(m), method = 'pearson', use = 'pairwise.complete.obs')
png('corr_cc.png', width = 2 * w, height = 2 * h)
corrplot(cm, type = 'upper', tl.cex = 2)
graphics.off()
sel = mat[, .SD, .SDcols = ! names(mat) %like% "lvl"]
m = sel[, .SD, .SDcols = ! names(sel) %like% "cc"]
cm = cor(as.matrix(m), method = 'pearson', use = 'pairwise.complete.obs')
png('corr.png', width = w, height = h)
corrplot(cm, type = 'upper', tl.cex = 2)
graphics.off()
