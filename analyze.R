as.numeric.factor <- function(x) {
    as.numeric(levels(x))[x]
}

                                        # not candidates to begin with
ignore = c('ms', 'n', 'm', 'proctime', 'comp', 'Problem', 'Planner',
           'Domain', 'Dom', 'Time', 'Steps', 'stepcount')
                                        # causing singularities
singularities = c('actions', 'ccdiameter1', 'ccradius1')
                                        # low significance
insignificant = c('ccdensity1', 'ccmaxeccentricity1',
                  'vertexcoverorder', 'ccn1',
                  'maxbetweennesscentrality', 'ccperccenter1',
                  'degreeassortativity', 'transitivity',
                  'maxclosenesscentrality')
                                        # low significance
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

raw = read.csv('stats.csv')
all = read.csv('IPCResults.csv')
valid = all[all$comp == 'IPC1998',] # leave out the IPC 1998 to use in the validation phase
expl = all[all$comp == 'IPC2000',] # use the IPC 2000 in the exploratory analysis
results = merge(x = expl, y = raw, by = 'Problem', all = FALSE)
validation = merge(x = valid, y = raw, by = 'Problem', all = FALSE)

rm(all)
rm(expl)
rm(raw)

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
    runtime = tedious$ms
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
            xlab("Runtime") +
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
    runtime = results$ms
    i = 1
    skip = c('Problem', 'Time', 'comp')
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
                xlab("Runtime") +
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
        png('clust_nlvl.png', width = w, height = h)
        par(mar = c(0,18,0,0))
        plot_horiz.dendrogram(dl, side = TRUE, sub="", main="", axes=F)
        invisible(dev.off())
        png('corr_nlvl.png', width = w, height = h)
        co = rev(groups$order)
        ocm = cm[co, co]
        signif = corr.test(keep, adjust="none")$p[co, co]
        ccolor = ifelse(c(!is.na(signif) & signif < 0.01), "black", "white")
        corrplot(ocm, type = 'upper', tl.cex = 0.5, cl.cex = 0.1, insig = "blank", method = "color", addCoef.col = ccolor, addCoefasPercent = TRUE)
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


