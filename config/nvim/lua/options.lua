-- line numbers
vim.wo.number = true
vim.o.relativenumber = true

-- highlight on search
vim.o.hlsearch = true

-- for clipboard to work with host OS clipboard
vim.opt.clipboard = 'unnamedplus'

-- case insensitive searching unless /C or capital in search
vim.o.ignorecase = true
vim.o.smartcase = true

-- mouse
vim.cmd("set mouse=nicr")
-- vim.o.mouse = '' [[disable mouse mode]]

-- fixes issues with colourschemes/opacity
vim.o.termguicolors = true

-- enable break indent
vim.o.breakindent = true

-- tabbing (using spaces for indenting)
vim.o.expandtab = true
vim.o.tabstop = 2
vim.o.shiftwidth = 2
vim.o.softtabstop = 2

-- decrease update time
vim.o.updatetime = 250

-- editor UI
vim.wo.signcolumn = 'yes'
vim.o.cursorline = true
vim.o.numberwidth = 2

-- undo history
vim.o.undofile = true 

-- leader key
-- must be before plugins (otherwise wrong leader will be used for plugins)
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

-- disable netrw (recommended nvim-tree settings)
vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1
