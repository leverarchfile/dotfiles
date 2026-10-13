return {
  "folke/tokyonight.nvim",
  lazy = false,    -- load at startup
  priority = 1000, -- load before all other plugins
  opts = {
    style = "night", -- "night", "storm", "moon", or "day"
    transparent = false, -- true to let the terminal background show through
    styles = {
      comments = { italic = false },
      keywords = { italic = false },
    },
  },
  config = function(_, opts)
    require("tokyonight").setup(opts)
    vim.cmd.colorscheme("tokyonight")
  end,
}
