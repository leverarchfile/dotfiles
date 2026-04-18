return {
  "kylechui/nvim-surround",
  version = "*", -- use for stability
  event = "VeryLazy",
  config = function()
    require("nvim-surround").setup({
    })
  end,
}
