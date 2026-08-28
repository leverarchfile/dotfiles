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

-- use underdotted style for diagnostic underlines instead of undercurl
vim.api.nvim_create_autocmd("ColorScheme", {
  callback = function()
    local function fix_underline(group)
      local hl = vim.api.nvim_get_hl(0, { name = group })
      hl.undercurl = nil
      hl.underdotted = true
      vim.api.nvim_set_hl(0, group, hl)
    end
    fix_underline("DiagnosticUnderlineError")
    fix_underline("DiagnosticUnderlineWarn")
    fix_underline("DiagnosticUnderlineInfo")
    fix_underline("DiagnosticUnderlineHint")

    -- prism: readable visual selection (black on bright lavender)
    vim.api.nvim_set_hl(0, "Visual", { bg = "#bda7f0", fg = "#000000" })

    -- prism: completion menu and floats on the surface tone
    vim.api.nvim_set_hl(0, "Pmenu",       { bg = "#303030", fg = "#bda7f0" })
    vim.api.nvim_set_hl(0, "PmenuSel",    { bg = "#ff7447", fg = "#000000" })
    vim.api.nvim_set_hl(0, "PmenuSbar",   { bg = "#303030" })
    vim.api.nvim_set_hl(0, "PmenuThumb",  { bg = "#989898" })
    vim.api.nvim_set_hl(0, "NormalFloat", { bg = "#303030", fg = "#bda7f0" })
    vim.api.nvim_set_hl(0, "FloatBorder", { bg = "#303030", fg = "#bda7f0" })
    vim.api.nvim_set_hl(0, "CmpItemAbbrMatch",      { fg = "#ff7447", bold = true })
    vim.api.nvim_set_hl(0, "CmpItemAbbrMatchFuzzy", { fg = "#ff7447", bold = true })

    -- prism: telescope as a selection list
    vim.api.nvim_set_hl(0, "TelescopeSelection",      { bg = "#ff7447", fg = "#000000", bold = true })
    vim.api.nvim_set_hl(0, "TelescopeSelectionCaret", { bg = "#ff7447", fg = "#000000" })
    vim.api.nvim_set_hl(0, "TelescopeMatching", {bold = true })

    -- prism: nvim-tree git-modified marker in amber
    vim.api.nvim_set_hl(0, "NvimTreeGitDirtyIcon", { fg = "#ffcc66" })

    -- prism: float titles in periwinkle, distinct from border and list
    vim.api.nvim_set_hl(0, "FloatTitle", { bg = "#303030", fg = "#989898" })
  end,
})
