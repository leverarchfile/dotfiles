return {
  "nvim-treesitter/nvim-treesitter",
  branch = "main",
  lazy = false,
  build = ":TSUpdate",

  config = function()
    local langs = {
      'c', 'lua', 'vim', 'vimdoc', 'query', 'elixir',
      'heex', 'javascript', 'html', 'python',
      'markdown', 'markdown_inline', 'yaml',
      'bash', 'css', 'go', 'json', 'toml', 'sql',
      'rust', 'regex', 'java',
    }
    require('nvim-treesitter').install(langs)

    -- main has no highlight module; start treesitter per buffer
    vim.api.nvim_create_autocmd('FileType', {
      callback = function(ev)
        pcall(vim.treesitter.start, ev.buf)
      end,
    })
  end,
}
