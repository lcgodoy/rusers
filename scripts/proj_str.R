# Project Structure

# Create folders

lapply(c('data', 'img', 'scripts', 'sections', 'style'), function(x) dir.create(path = x))

# Create files

lapply(file.path('sections', 
                 c('01-intro.Rmd', '02-vdl.Rmd', '03-applic.Rmd', 
                   '04-results.Rmd', '05-conclusion.Rmd')), file.create)

# Copy css's

lapply(list.files(path = '../datathon/assets', full.names = T), function(x) {
    file.copy(from = x, to = gsub(pattern = '../datathon/assets', replacement = 'style', x = x))
})

file.copy(from = '../rbras/img/voronoi_ex.gif', to = 'img/vor_ex.gif')

file.copy(from = '../../csds1_analises/data/processed/data_comparison.RData', to = 'data/data_comparison.RData')
file.copy(from = '../../csds1_analises/data/processed/ibge_sp.RData', to = 'data/ibge_sp.RData')
