-- files
-- vim.api.nvim_set_keymap("n", "QQ", ":q!<enter>", {noremap=false})
-- vim.api.nvim_set_keymap("n", "FF", ":w!<enter>", {noremap=false})

vim.api.nvim_set_keymap("n", "<leader>fd", ":q!<enter>", {noremap=false})
vim.api.nvim_set_keymap("n", "<leader>ff", ":w!<enter>", {noremap=false})

-- view marks in Telescope
vim.api.nvim_set_keymap("n", "MM", ":Telescope marks<enter>", {noremap=false})

-- buffers
vim.api.nvim_set_keymap("n", "tj", ":bnext<enter>", {noremap=false})
vim.api.nvim_set_keymap("n", "tk", ":bprev<enter>", {noremap=false})
vim.api.nvim_set_keymap("n", "th", ":bfirst<enter>", {noremap=false})
vim.api.nvim_set_keymap("n", "tl", ":blast<enter>", {noremap=false})
vim.api.nvim_set_keymap("n", "td", ":bdelete<enter>", {noremap=false})

-- new vertical split
vim.keymap.set("n", "<leader>ws", ":vsplit<CR><C-w>w", {noremap=false})

-- navigating split windows

-- close split
vim.keymap.set("n", "<leader>wq", ":q<CR>",{silent = true, noremap = true})
