-- files
vim.keymap.set("n", "<leader>fd", ":q!<enter>", {noremap=false})
vim.keymap.set("n", "<leader>ff", ":w!<enter>", {noremap=false})

-- view marks in Telescope
vim.keymap.set("n", "MM", ":Telescope marks<enter>", {noremap=false})

-- buffers
vim.keymap.set("n", "tj", ":bnext<enter>", {noremap=false})
vim.keymap.set("n", "tk", ":bprev<enter>", {noremap=false})
vim.keymap.set("n", "th", ":bfirst<enter>", {noremap=false})
vim.keymap.set("n", "tl", ":blast<enter>", {noremap=false})
vim.keymap.set("n", "td", ":bdelete<enter>", {noremap=false})

-- new vertical split
vim.keymap.set("n", "<leader>ws", ":vsplit<CR><C-w>w", {noremap=false})

-- navigating split windows

-- close split
vim.keymap.set("n", "<leader>wq", ":q<CR>",{silent = true, noremap = true})
