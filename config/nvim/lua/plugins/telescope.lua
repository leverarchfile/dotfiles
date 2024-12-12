
vim.keymap.set('n', '<leader>fs', require('telescope.builtin').find_files, { desc = '[S]earch [F]iles' })
vim.keymap.set('n', '<leader>fg', require('telescope.builtin').live_grep, { desc = '[S]earch by [G]rep' })

vim.keymap.set('n', '<leader>fb', require('telescope.builtin').buffers, { desc = '[ ] Find existing buffers' })

vim.keymap.set('n', '<leader>sm', ":Telescope harpoon marks<CR>", { desc = 'Harpoon [M]arks' })
