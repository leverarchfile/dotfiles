require('nvim-treesitter.configs').setup {
  -- languages for treesitter
  ensure_installed = {
    'c', 'lua', 'vim', 'vimdoc', 'query', 'elixir',
    'heex', 'javascript', 'html', 'python', 'org',
    'markdown', 'markdown_inline', 'yaml',
    'bash', 'css', 'go', 'json', 'toml', 'sql',
    'rust', 'regex', 'java',
  },

  sync_install = false,
  highlight = { enable = true },
  indent = { enable = true },
}
