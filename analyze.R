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
redrawScatter = FALSE
redrawCorr = FALSE
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
ipc = read.csv('IPCResults.csv')
results = merge(x = ipc, y = raw, by = 'Problem', all = FALSE)

rm(ipc)
rm(raw)
vars = ncol(results)
library(dplyr) # filter
library(ggplot2)
library(ggrepel)
require(gridExtra)
require(cowplot)
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
}

                                        # normalizations based on scatter plots
results$vertexcoverorder = results$vertexcoverorder / results$n
results$edgecoverorder = results$edgecoverorder / results$n # counterintuitive, but yes
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


