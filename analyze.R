suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
suppressMessages(library(ggrepel))
suppressMessages(require(gridExtra))
suppressMessages(require(cowplot))
suppressMessages(require(scales))
suppressMessages(require(viridis))
suppressMessages(require(corrplot))
suppressMessages(require(psych))
suppressMessages(require(data.table))
suppressMessages(require(gdata))

options(scipen=10000)

as.numeric.factor <- function(x) {
    as.numeric(levels(x))[x]
}

minObs = 100
minUniq = 5
redoExpl = TRUE
redrawScatter = TRUE
redrawViolin = TRUE
redrawCorr = TRUE
remakeFormula = TRUE

data = read.csv('stats.csv')
data$Planner = factor(data$Planner)
data$Dom = factor(data$Dom)
data$Comp = factor(data$Comp)
data = data[data$order > 0,] # do not include unconcluded runs
data = data[data$Stage <= 5,] # if more was extracted or characterized, do not include it
data$Stage = factor(data$Stage)

print(summary(data$Comp))

for (co in levels(data$Comp)) {
    subset = data %>% filter(Comp == co)
    subset =  droplevels(subset)
    count = length(levels(subset$Dom))
    p = ggplot(subset, aes(x = Dom, y = Time, fill = Dom))
    p = p + geom_violin(trim = FALSE) + geom_boxplot(width=0.1)
    p = p + xlab('Problem domain') + ylab('Reported runtime in milliseconds')
    p = p + theme(legend.position="none")
    p = p + scale_y_continuous(trans='log2', labels = comma)
    p = p + theme_bw() # no gray background
    filename = sprintf('domains_%s.pdf', co)
    ggsave(filename, width = 3 * count, height = 10, units = "cm")
    count = length(levels(subset$Planner))
    p = ggplot(subset, aes(x = Planner, y = Time, fill = Planner))
    p = p + geom_violin(trim = FALSE) + geom_boxplot(width=0.1)
    p = p + xlab('Planner') + ylab('Reported runtime in milliseconds')
    p = p + theme(legend.position="none")
    p = p + scale_y_continuous(trans='log2', labels = comma)
    p = p + theme_bw() # no gray background
    filename = sprintf('planners_%s.pdf', co)
    ggsave(filename, width = 10 * log(count), height = 12, units = "cm")
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
        scale_fill_viridis(trans = 'log10', option = "inferno",
                           discrete = FALSE, na.value = 'gray',
                           limits = lims, breaks = br) +
        xlab('Problem domain') +
        labs(fill = 'Quantile')
    theme_bw()
    filename = sprintf('heatmap_%s.pdf', co)
    ggsave(filename, width = 2 * 3 * qdw, height = 2 * qdh, units = "cm")
}

sh = hash()
for (stage in 1:5) {
    stageData = data %>% filter(Stage == stage)
    p = ggplot(stageData, aes(x = order, y = Time)) + ggtitle(sprintf('Stage %d', stage)) +
        geom_point(aes(color = Dom, shape = Planner, size = size),
                   alpha = 0.6, position = position_jitter(w = 0.05, h = 0)) +
        scale_shape_manual(values=1:nlevels(data$Planner)) +
        xlab('Order') + ylab('Total runtime') +
        labs(color = 'Domain', shape = 'Planner', size = 'Size') +
        scale_x_continuous(trans = 'log2', labels = comma, limits = c(4, 512)) +
        scale_y_continuous(trans='log2', labels = comma) +
        theme_bw()
    if (stage < 5) {
        sh[[toString(stage)]] = p + guides(color = FALSE, shape = FALSE) +
            theme(legend.position = "top", legend.box = "horizontal")
    } else {
        sh[[toString(stage)]] = p + guides(size = guide_legend(order = 1),
                                           color = guide_legend(order = 2),
                                           shape = guide_legend(order = 3)) +
            theme(legend.position = "right", legend.box = "vertical")
    }
}
png("scatter.png", width = 2000, height = 600)
grid.arrange(sh[["1"]], sh[["2"]], sh[["3"]], sh[["4"]], sh[["5"]], nrow = 1, widths = c(rep(1, 4), 1.3))
dev.off()

results = data %>% filter(Comp == 'IPC2000') # leave out the IPC 1998 to use in the validation phase
print(summary(results$Comp))
print(names(results))
print(summary(results$Time))
print(summary(results$order))
print(summary(results$size))
print(summary(results$Steps))

model = lm(log(Time + 1) ~ log(order) * log(size) * Stage, data = results)
s = summary(model)
options(digits=3)
sink('model.txt')
print(s)
sink()
print(s$adj.r.squared)
results$modpred = predict(model)
results$predicted = exp(results$modpred) - 1
results$unexplained = results$Time - results$predicted # negative if easier than expected

cat(c('total', dim(results)[1]), '\n')
easy = filter(results, results$predicted > 2 * results$Time)
cat(c('easy', dim(easy)[1]), '\n')
hard = filter(results, results$predicted < results$Time / 2)
cat(c('hard', dim(hard)[1]), '\n')
regular = filter(results, ((results$predicted >= results$Time / 2) & (results$predicted <= 2 * results$Time)))
cat(c('regular', dim(regular)[1]), '\n')

if (redoExpl) {
   p = ggplot(results, aes(x = order, y = Time)) # basic measures: predicted versus unexplained runtime
   p = p + geom_point(aes(size = size, color = Planner), alpha = 0.2)
   p = p + xlab('Number of nodes') + ylab('Reported runtime in milliseconds')
   p = p + labs(color = 'Number of levels', size = 'Number of edges')
   p = p + scale_x_continuous(trans = 'log2', labels = comma) + scale_y_continuous(trans='log2', labels = comma)
#   p = p + xlim(1, 2000) + ylim(1, 3000000)
   p = p + scale_color_gradient(low="blue", high="red")
   p = p + theme_bw() # no gray background
   ggsave("defaults.pdf", width = 20, height = 20, units = "cm")
   p = ggplot(results, aes(x = order, y=predicted)) + geom_point(aes(size = size, color = Planner, shape = Dom), alpha = 0.5)
   p = p + scale_shape_manual(values=1:nlevels(results$Dom))
   p = p + xlab('Number of nodes') + ylab('Predicted runtime in milliseconds')
   p = p + labs(color = 'Number of levels', size  = 'Number of edges', shape = 'Problem domain')
   p = p + scale_x_continuous(trans = 'log2', labels = comma) + scale_y_continuous(trans='log2', labels = comma)
#   p = p + xlim(1, 2000) + ylim(1, 100000)
   p = p + scale_color_gradient(low="blue", high="red")
   p = p + theme_bw() # no gray background
   ggsave("explained.pdf", width = 20, height = 20, units = "cm")
   p = ggplot(results, aes(x = order, y=unexplained)) + geom_point(aes(size = size, color = Planner, shape = Dom), alpha = 0.5)
   p = p + scale_shape_manual(values=1:nlevels(results$Dom))
   p = p + xlab('Number of nodes') + ylab('Unexplained runtime in milliseconds')
   p = p + labs(color = 'Number of levels', size  = 'Number of edges', shape = 'Problem domain')
   p = p + scale_x_continuous(trans = 'log2', labels = comma)
   p = p + scale_y_continuous(trans='log2', labels = comma)
#   p = p + xlim(1, 2000) + ylim(1, 3000000)
   p = p + scale_color_gradient(low="blue", high="red")
   p = p + theme_bw() # no gray background
   ggsave("unexplained.pdf", width = 20, height = 20, units = "cm")
   vb = seq(250, 1250, 250)
   eb = seq(20000, 60000, 20000)
   p = ggplot(easy, aes(x = Time, y=-unexplained)) + geom_point(aes(size = size, color = order), alpha = 0.5)
   p = p + xlab('Actual runtime in milliseconds') + ylab('"Missing" runtime in milliseconds')
   p = p + labs(size  = 'Number of edges', color = 'Number of vertices')
   p = p + scale_x_continuous(trans='log2', labels = comma)
   p = p + scale_y_continuous(trans='log2', labels = comma)
#   p = p + xlim(1, 2000) + ylim(1, 3000000)
   p = p + scale_color_gradient(low="blue", high="red", breaks = vb) + scale_size(breaks = eb)
   p = p + theme_bw() # no gray background
   ggsave("easy.pdf", width = 15, height = 17, units = "cm")
   p = ggplot(hard, aes(x = Time, y=unexplained)) + geom_point(aes(size = size, color = order), alpha = 0.5)
   p = p + xlab('Actual runtime in milliseconds') + ylab('Unexplained runtime in milliseconds')
   p = p + labs(size  = 'Number of edges', color = 'Number of vertices')
   p = p + scale_x_continuous(trans='log2', labels = comma)
   p = p + scale_y_continuous(trans='log2', labels = comma)
#   p = p + xlim(1, 2000) + ylim(1, 3000000)
   p = p + scale_color_gradient(low="blue", high="red", breaks = vb) + scale_size(breaks = eb)
   p = p + theme_bw() # no gray background
   ggsave("hard.pdf", width = 15, height = 17, units = "cm")
   p = ggplot(regular, aes(x=Time, y=unexplained)) + geom_point(aes(size = size, color = order), alpha = 0.5)
   p = p + xlab('Actual runtime in milliseconds') + ylab('Unexplained runtime in milliseconds')
   p = p + labs(size  = 'Number of edges', color = 'Number of vertices')
   p = p + scale_x_continuous(trans='log2', labels = comma)
#   p = p + xlim(1, 2000) + ylim(1, 3000000)
   p = p + scale_color_gradient(low="blue", high="red", breaks = vb) + scale_size(breaks = eb)
   p = p + theme_bw() # no gray background
   ggsave("regular.pdf", width = 15, height = 17, units = "cm")
}

vars = ncol(results)
results = filter(results, !is.na(results$ms))
skip = c('ms', 'n', 'm', 'proctime',
           'Comp', 'Problem', 'Planner',
           'Domain', 'Dom', 'Time', 'Stage',
           'Steps', 'unexplained', 'modpred', 'predicted') # not potential structural measures
listing = names(results)
targets = listing[!listing  %in% skip]

# TO BE DONE: this should probably happen by stage
alpha = 0.01
if (redrawViolin) {
    for (target in targets) {
        values = list(easy[[target]], regular[[target]], hard[[target]])
        t = data.frame(Value = numeric(), Class = numeric())
        for (i in 1:3) {
            for (value in values[[i]]) {
                t = rbind(t, c(value, i))
            }
        }
        names(t) = c("Value", "Class")
        t$Class = as.factor(t$Class)
        t$Class = recode_factor(t$Class, "1" = "easy", "2" = "regular", "3" = "hard")
        t = drop.levels(t[complete.cases(t), ]) # skip NA and reduce factors
        cat(target, summary(t$Class), '\n')
        if (length(levels(t$Class)) > 1 && dim(t)[1] > 30) {
            kwt = kruskal.test(t$Value~t$Class)
            p = kwt$p.value
            if (!is.nan(p) && p < alpha) { # differently distributed
                vp = ggplot(t, aes(x=Class, y=Value, fill=Class)) +
                    labs(title = sprintf('Characteristic %s (p = %.5f)', target, p)) +
                    geom_violin(trim=FALSE) +
                    scale_fill_manual(values=c("#66ff66", "#6666ff", "#ff6666"))
                ggsave(sprintf("violin_%s.pdf", target), width = 12, height = 7, units = "cm")
            }
        }
    }
}


if (redrawScatter) {
    cutoff = 200000
    tedious = filter(results, results$Time > cutoff)
    cat(dim(tedious))
    order = tedious$order
    size = tedious$size
    runtime = tedious$unexplained # study ONLY the unexplained runtime
    planner = tedious$Planner
    domain = tedious$Dom
    shapes = 1:nlevels(tedious$Dom)
    a = 0.7
    ps = 3
    ts = 2
    for (target in targets) {
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
        png(sprintf("s_nmt_%s.png", target), width = 1200, height = 1200)
        grid.arrange(p1, p2, p3, legend, top = target)
        dev.off()
    }
    shapes = 1:nlevels(results$Dom)
    order = results$n
    size = results$m
    runtime = results$unexplained
    i = 1
    for (target in targets) {
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
print(summary(results$Comp))


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

validation = data %>% filter(Comp == 'IPC2000') # use the IPC 2000 in the exploratory analysis


