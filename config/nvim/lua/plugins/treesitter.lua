return {
  "nvim-treesitter/nvim-treesitter", 
  lazy = false,
  build = ":TSUpdate",

  config = function()

    require('nvim-treesitter').setup {
      -- languages for treesitter
      ensure_installed = {
        'c', 'lua', 'vim', 'vimdoc', 'query', 'elixir',
        'heex', 'javascript', 'html', 'python', 'org',
        'markdown', 'markdown_inline', 'yaml',
        'bash', 'css', 'go', 'json', 'toml', 'sql',
        'rust', 'regex', 'java',
      },
    }
  end,
}
