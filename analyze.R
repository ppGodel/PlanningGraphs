as.numeric.factor <- function(x) {
    as.numeric(levels(x))[x]
}

                                        # not candidates to begin with
ignore = c('ms', 'n', 'm', 'proctime', 'comp', 'Problem', 'Planner',
           'Domain', 'Dom', 'Time', 'Steps', 'stepcount')
                                        # causing singularities UPDATE THESE
singularities = c('actions', 'ccdiameter1', 'ccradius1')
                                        # low significance UPDATE THESE
insignificant = c('ccdensity1', 'ccmaxeccentricity1',
                  'vertexcoverorder', 'ccn1',
                  'maxbetweennesscentrality', 'ccperccenter1',
                  'degreeassortativity', 'transitivity',
                  'maxclosenesscentrality')
                                        # low significance UPDATE THESE
low = c('maxloadcentrality', 'ccmaxrichclub1', 'ccm1',
        'ccpercperiphery1')
skip = c(ignore, singularities, insignificant, low)

minObs = 100
minUniq = 5
redrawScatter = TRUE
redrawCorr = TRUE
remakeFormula = FALSE

process <- function(df) {
    formula = 'ms ~ m'
    print(summary(df$ms))
    print(summary(df$n))
    print(summary(df$m))
    d = dim(df)
    n = d[1]
    k = d[2]
    used = numeric()
    for (i in 1 : k) {
        ok = TRUE
        s = names(df)[i]
        if (!(s %in% skip)) {
            column = df[, i]
            if (class(column) == "factor") {
                k = length(levels(column))
                ok = (k > 1)
                if (!ok) {
                    cat('skipping factor', s, '\n')
                }
            } else { # numeric
                present = n - sum(is.na(column))
                if (present < minObs) {
                    ok = FALSE
                } else {
                    stddev = sd(column, na.rm = TRUE)
                    ok = (stddev > 0)
                }
                if  (!ok) {
                    cat('skipping numerical attribute', s, '\n')
                }
            }
            if (ok) {
                cat(i, 'Including', s, class(column), '\n')
                used = c(used, i)
                formula = paste(formula, s, sep=' + ')
            }
        }
    }
    options(width = 1000)
    sink('output.txt')
    interact = gsub('\\+', '*', formula) # the original was a linear model
    anova = aov(as.formula(interact), data = df, na.action = na.exclude)
    print(summary(anova))
    sink()
    options(width = 100)
    return(used)
}

all = read.csv('IPCResults.csv') # IPC results
raw = read.csv('stats.csv') # our measurements
combo = merge(x = all, y = raw, by = 'Problem', all = FALSE)
require(dplyr)
results = combo %>% filter(comp == 'IPC1998') # leave out the IPC 1998 to use in the validation phase
validation = combo %>% filter(comp == 'IPC2000') # use the IPC 2000 in the exploratory analysis
require(ggplot2) # start intro plots
require(scales)
library("viridis")
options(scipen=10000)
for (co in levels(all$comp)) {
    subset = all %>% filter(comp == co)
    subset =  droplevels(subset)
    count = length(levels(subset$Dom))
    p = ggplot(subset, aes(x = Dom, y = Time, fill = Dom))
    p = p + geom_violin(trim = FALSE) + geom_boxplot(width=0.1)
    p = p + xlab('Problem domain') + ylab('Reported runtime in milliseconds')
    p = p + theme(legend.position="none")
    p = p + scale_y_continuous(trans='log2', labels = comma)
    p = p + theme_bw() # no gray background
    filename = sprintf('domains_%s.pdf', co)
    ggsave(filename, width = 2 * count, height = 10, units = "cm")
    count = length(levels(subset$Planner))
    p = ggplot(subset, aes(x = Planner, y = Time, fill = Planner))
    p = p + geom_violin(trim = FALSE) + geom_boxplot(width=0.1)
    p = p + xlab('Planner') + ylab('Reported runtime in milliseconds')
    p = p + theme(legend.position="none")
    p = p + scale_y_continuous(trans='log2', labels = comma)
    p = p + theme_bw() # no gray background
    filename = sprintf('planners_%s.pdf', co)
    ggsave(filename, width = 2 * count, height = 10, units = "cm")
    qdw = length(levels(subset$Dom))
    qdh = length(levels(subset$Planner))
    count = 3 * qdh * qdw
    qd = data.frame(Dom = rep("", count), Planner = rep("", count),
                    quantile = rep(NA, count),
                    value = rep(NA, count),
                    stringsAsFactors=FALSE)
    pos = 1
    for (d in levels(subset$Dom)) {
        subset2 = subset %>% filter(Dom == d)
        for (p in levels(subset2$Planner)) {
            subset3 = subset2 %>% filter(Planner == p)
            times = subset3$Time
            times = times[!is.na(times)]
            sumd = summary(times)
            qd[pos, ] = list(d, p, 25, as.numeric(sumd[2])) # first
            pos = pos + 1
            qd[pos, ] = list(d, p, 50, as.numeric(sumd[3])) # second
            pos = pos + 1
            qd[pos, ] = list(d, p, 75, as.numeric(sumd[5])) # third
            pos = pos + 1
        }
    }
    qd = qd[1:(pos-1),]
    low = min(qd$value[!is.na(qd$value)])
    high =max(qd$value[!is.na(qd$value)])
    lp = floor(log10(low))
    hp = ceiling(log10(high))
    br = 10^seq(lp, hp, by = 1)
    lims = c(10^lp, 10^hp)
    p = ggplot(qd, aes(x = Dom, y = Planner, fill = value)) +
        facet_grid(cols = vars(quantile)) +
        geom_tile() +
        scale_fill_viridis(trans = 'log10', option = "inferno", discrete = FALSE, na.value = 'gray', limits = lims, breaks = br) +
        xlab('Problem domain') +
        labs(fill = 'Quantile')
        theme_bw()
    filename = sprintf('heatmap_%s.pdf', co)
    ggsave(filename, width = 2 * 3 * qdw, height = 2 * qdh, units = "cm")
}

p = ggplot(combo, aes(x = n, y = Time))
p = p + geom_point(aes(color = Dom, shape = Planner, size = m), alpha = 0.6, position = position_jitter(w = 0.05, h = 0))
p = p + scale_shape_manual(values=1:nlevels(combo$Planner))
p = p + xlab('Graph order') + ylab('Runtime')
p = p + labs(color = 'Domain', shape = 'Planner', size  = 'Graph size')
p = p + theme(legend.box = "horizontal")
p = p + scale_x_continuous(trans = 'log2', labels = comma) + scale_y_continuous(trans='log2', labels = comma)
p = p + theme_bw() # no gray background
ggsave("scatter.pdf", width = 30, height = 20, units = "cm")


quit()






# basic measures: explained versus unexplained runtime
p = ggplot(results, aes(x=n, y=Time))
p = p + geom_point(aes(size = m, color = stepcount), alpha = 0.5)
p = p + xlab('Number of nodes') + ylab('Reported runtime in milliseconds')
p = p + labs(color = 'Number of levels', size  = 'Number of edges')
p = p + scale_x_continuous(trans = 'log2', labels = comma) + scale_y_continuous(trans='log2', labels = comma)
p = p + xlim(1, 2000) + ylim(1, 3000000)
p = p + scale_color_gradient(low="blue", high="red")
p = p + theme_bw() # no gray background
ggsave("defaults.pdf", width = 20, height = 20, units = "cm")
model = lm(log(Time + 1) ~ log(n) * sqrt(m) * stepcount, data = results)
s = summary(model)
options(digits=3)
sink('model.txt')
print(s)
sink()
print(s$adj.r.squared)
results$explained = predict(model)
p = ggplot(results, aes(x=n, y=explained)) + geom_point(aes(size = m, color = stepcount), alpha = 0.5)
p = p + xlab('Number of nodes') + ylab('Explained runtime in milliseconds')
p = p + labs(color = 'Number of levels', size  = 'Number of edges')
p = p + scale_x_continuous(trans = 'log2', labels = comma) + scale_y_continuous(trans='log2', labels = comma)
p = p + xlim(1, 2000) + ylim(1, 3000000)
p = p + scale_color_gradient(low="blue", high="red")
p = p + theme_bw() # no gray background
ggsave("explained.pdf", width = 20, height = 20, units = "cm")
results$unexplained = results$Time - results$explained
p = ggplot(results, aes(x=n, y=unexplained)) + geom_point(aes(size = m, color = stepcount), alpha = 0.5)
p = p + xlab('Number of nodes') + ylab('Unexplained runtime in milliseconds')
p = p + labs(color = 'Number of levels', size  = 'Number of edges')
p = p + scale_x_continuous(trans = 'log2', labels = comma) + scale_y_continuous(trans='log2', labels = comma)
p = p + xlim(1, 2000) + ylim(1, 3000000)
p = p + scale_color_gradient(low="blue", high="red")
p = p + theme_bw() # no gray background
ggsave("unexplained.pdf", width = 20, height = 20, units = "cm")


vars = ncol(results)
suppressMessages(library(dplyr)) # filter
suppressMessages(library(ggplot2))
suppressMessages(library(ggrepel))
suppressMessages(require(gridExtra))
suppressMessages(require(cowplot))
results = filter(results, !is.na(results$ms))
results$Planner = factor(results$Planner)
results$Dom = factor(results$Dom)

if (redrawScatter) {
    cutoff = 200000
    tedious = filter(results, results$ms > cutoff)
    order = tedious$n
    size = tedious$m
    runtime = tedious$unexplained # study ONLY the unexplained runtime
    planner = tedious$Planner
    domain = tedious$Dom
    shapes = 1:nlevels(tedious$Dom)
    a = 0.7
    ps = 3
    ts = 2
    # 68 is avg degree conn and it was not computed for any of the tedious ones and is hence not drawn
    special = c(12, 16, 17, 20, 21, 23, 38, 44, 52, 55, 58, 61)
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
            xlab("Unexplained runtime") +
            ylab(target) +
            geom_text_repel(tedious, mapping = aes(label = Problem), size = ts, hjust = 0, nudge_x = 5)
        legend = cowplot::get_legend(p0)
        png(sprintf("s_nmt_%d.png", s), width = 1200, height = 1200)
        grid.arrange(p1, p2, p3, legend, top = target)
        dev.off()
    }
    planner = factor(results$Planner)
    domain = factor(results$Dom)
    shapes = 1:nlevels(results$Dom)
    order = results$n
    size = results$m
    runtime = results$unexplained
    i = 1
    targets = names(results)
    for (target in targets[!targets  %in% skip]) {
        if (length(unique(results$target)) >= minUniq) { # enough unique values for the vertical axis
            cat('creating a scatter plot for', target, '\n')
            p0 = ggplot(results, aes_string(x = order, y = size)) +
                geom_point(aes(color = planner, shape = domain)) +
                scale_shape_manual(values = shapes) +
                theme(legend.box = "horizontal") +
                guides(fill=guide_legend(ncol=4))
            p1 = ggplot(results, aes_string(x = order, y = target)) +
                geom_point(aes(color = planner, shape = domain), size = ps, alpha = a) +
                scale_shape_manual(values = shapes) +
                theme(legend.position = "none") +
                xlab("Order") +
                ylab(target)
            p2 = ggplot(results, aes_string(x = size, y = target)) +
                geom_point(aes(color = planner, shape = domain), size = ps, alpha = a) +
                scale_shape_manual(values = shapes) +
                theme(legend.position = "none") +
                xlab("Size") +
                ylab(target)
            p3 = ggplot(results, aes_string(x = runtime, y = target)) +
                geom_point(aes(color = planner, shape = domain), size = ps, alpha = a) +
                scale_shape_manual(values = shapes) +
                theme(legend.position = "none") +
                xlab("Unexplained runtime") +
                ylab(target)
            legend = cowplot::get_legend(p0)
            png(sprintf("nmt_%d.png", i), width = 900, height = 900)
            i = i + 1
            grid.arrange(p1, p2, p3, legend, top = target)
            dev.off()
        } else {
            cat('skipping unhelpful scatter plot for', target, '\n')
        }
    }
}
if (redrawCorr) {
    cc = read.csv('components.csv')
    cc$stepcount = NULL # redundant
    cc$ms = NULL
    tmp = merge(x = results, y = cc, by = 'Problem', all = FALSE)
    rm(cc)
    layers = read.csv('layers.csv')
    layers$stepcount = NULL # redundant
    layers$ms = NULL
    comb = merge(x = tmp, y = layers, by = 'Problem', all = FALSE)
    rm(tmp)
    rm(layers)
    cat(names(comb)[1:6], '\n')
    cat(sapply(comb, typeof)[1:6], '\n')
    cat(sapply(comb, class)[1:6], '\n')
    suppressMessages(library("corrplot"))
    suppressMessages(library("psych"))
    suppressMessages(library("data.table"))
    factors = c(1, 2, 3, 6) # non-numeric measurements
    mat = as.data.table(comb[, -factors]) # remove factors
    w = 2000
    h = 2000
    lvlm = mat[, .SD, .SDcols = names(mat) %like% "lvl"]
    for (lvl in 0:3) {
        label = sprintf("lvl%d", lvl)
        cat(label, '\n')
        sel = lvlm[, .SD, .SDcols = names(lvlm) %like% label]
        mcc = sel[, .SD, .SDcols = names(sel) %like% "cc"]
        omit = c()
        for (i in names(mcc)) {
            values = mcc[[i]]
            values = values[!is.na(values)]
            if (length(values) < 2 || sd(values) == 0) {
                omit = c(omit, i)
            }
        }
        cat('omitting', length(omit), 'from a total of', length(names(mcc)), 'for having zero std dev\n')
        ok = !(names(mcc) %in% omit)
        keep = mcc[ , ..ok]
        cm = cor(keep, method = 'pearson', use = 'pairwise.complete.obs')
        if (dim(cm)[1] > 1) { # if there are enough to analyze
            groups = hclust(dist(abs(cm))) # cluster them
            suppressMessages(suppressWarnings(library(dendextend)))
            d = as.dendrogram(groups)
            db = color_branches(d, k = 4)
            dl = color_labels(db, k = 4)
            png(sprintf('clust_cc_%s.png', label), width = w, height = h)
            par(mar = c(0,18,0,0))
            plot_horiz.dendrogram(dl, side = TRUE, sub="", main="", axes=F)
            invisible(dev.off())
            filename = sprintf('corr_cc%s.png', label)
            cat(filename, '\n')
            png(filename, width = (5 - lvl) * w, height = (5 - lvl) * h)
            co = rev(groups$order)
            ocm = cm[co, co]
            signif = corr.test(keep, adjust="none")$p[co, co]
            ccolor = ifelse(c(!is.na(signif) & signif < 0.01), "black", "white")
            corrplot(ocm, type = 'upper', tl.cex = 0.5, cl.cex = 0.1, insig = "blank", method = "color", addCoef.col = ccolor, addCoefasPercent = TRUE)
            graphics.off()
        } else {
            cat('level',  lvl, 'has too few variable measures for a cc corrplot\n')
        }
        mncc = sel[, .SD, .SDcols = ! names(sel) %like% "cc"]
        omit = c()
        for (i in names(mncc)) {
            values = mncc[[i]]
            values = values[!is.na(values)]
            if (length(values) < 2 || sd(values) == 0) {
                omit = c(omit, i)
            }
        }
        cat('omitting', length(omit), 'from a total of', length(names(mncc)), 'for having zero std dev\n')
        ok = !(names(mncc) %in% omit)
        keep = mncc[ , ..ok]
        cm = cor(keep, method = 'pearson', use = 'pairwise.complete.obs')
        if (dim(cm)[1] > 1) {
            groups = hclust(dist(abs(cm)))
            suppressMessages(suppressWarnings(library(dendextend)))
            d = as.dendrogram(groups)
            db = color_branches(d, k = 4)
            dl = color_labels(db, k = 4)
            png(sprintf('clust_ncc_%s.png', label), width = w, height = h)
            par(mar = c(0,18,0,0))
            plot_horiz.dendrogram(dl, side = TRUE, sub="", main="", axes=F)
            invisible(dev.off())
            filename = sprintf('corr_%s.png', label)
            png(filename, width = w, height = h)
            co = rev(groups$order)
            ocm = cm[co, co]
            signif = corr.test(keep, adjust="none")$p[co, co]
            ccolor = ifelse(c(!is.na(signif) & signif < 0.01), "black", "white")
            corrplot(ocm, type = 'upper', tl.cex = 0.5, cl.cex = 0.1, insig = "blank", method = "color", addCoef.col = ccolor, addCoefasPercent = TRUE)
            graphics.off()
        } else {
            cat('level',  lvl, 'has too few variable measures for a non-cc corrplot\n')
        }
    }
    sel = mat[, .SD, .SDcols = names(mat) %like% "cc"]
    mnlvl = sel[, .SD, .SDcols = ! names(sel) %like% "lvl"]
    omit = c()
    for (i in names(mnlvl)) {
        values = mnlvl[[i]]
        values = values[!is.na(values)]
        if (length(values) < 2 || sd(values) == 0) {
            omit = c(omit, i)
        }
    }
    cat('omitting', length(omit), 'from a total of', length(names(mncc)), 'for having zero std dev\n')
    ok = !(names(mnlvl) %in% omit)
    keep = mnlvl[ , ..ok]
    cm = cor(keep, method = 'pearson', use = 'pairwise.complete.obs')
    if (dim(cm)[1] > 1) {
        groups = hclust(dist(abs(cm)))
        suppressMessages(suppressWarnings(library(dendextend)))
        d = as.dendrogram(groups)
        db = color_branches(d, k = 4)
        dl = color_labels(db, k = 4)
        png('clust_nlvl.png', width = w / 2, height = h)
        par(mar = c(0,18,0,0))
        plot_horiz.dendrogram(dl, side = TRUE, sub="", main="", axes=F)
        invisible(dev.off())
        png('corr_nlvl.png', width = w, height = h)
        co = rev(groups$order)
        ocm = cm[co, co]
        signif = corr.test(keep, adjust="none")$p[co, co]
        ccolor = ifelse(c(!is.na(signif) & signif < 0.01), "black", "white")
        corrplot(ocm, type = 'upper', tl.cex = 1, cl.cex = 0.2, insig = "blank", method = "color", addCoef.col = ccolor, addCoefasPercent = TRUE)
        graphics.off()
    }
}

                                        # normalizations based on visual inspection of the scatter plots
results$vertexcoverorder = results$vertexcoverorder / results$n
results$edgecoverorder = results$edgecoverorder / results$n # perhaps counterintuitive, but necessary
results$ccn1 = results$ccn1 / results$n
results$ccm1 = results$ccn1 / results$m
results$actions = results$actions / results$n
results$facts = results$facts / results$n
results$maxtriangles = results$maxtriangles / results$m
results$maxavgdegconnectivity = results$maxavgdegconnectivity / (results$n)^2
results$maxeigenvectorcentrality = results$maxeigenvectorcentrality * (results$n)
results$greedycolors = results$greedycolors / results$n
print(summary(results$maxtriangles))
print(summary(results$comp))

limit = 500
working = filter(results, results$ms > limit)
if (remakeFormula) {
    print(process(working))
    system('bash formula.sh')
}
library(readr)
formula = trimws(read_file("formula.txt"))
keep = c('ms', unlist(strsplit(formula, " \\* ")))
formula = paste('ms ~ ', formula)
selected = names(working) %in% keep
use = working[, selected]
use = select(working, ms, avgclustering, edgecoverorder, facts, greedycolors, m, maxavgdegconnectivity, maxeigenvectorcentrality, maxtriangles)
use = use[complete.cases(use), ]
f = as.formula(formula)
print(dim(use))
print(f)
options(width = 500)
anova = aov(f, data = use)
print(summary(anova))
model = lm(f, data = use)
s = summary(model)
print(s)
print(s$adj.r.squared)


