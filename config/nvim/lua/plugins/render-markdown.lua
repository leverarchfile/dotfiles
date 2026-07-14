return {
  "MeanderingProgrammer/render-markdown.nvim",
  dependencies = { "nvim-treesitter/nvim-treesitter", "nvim-tree/nvim-web-devicons" },
  ft = { "markdown" },
  config = function()
    local function heading_backgrounds()
      local ok, pywal = pcall(require, "pywal16.core")
      if not ok then return end
      local c = pywal.get_colors()
      local accents = { c.color1, c.color2, c.color3, c.color4, c.color5, c.color6 }
      for i, bg in ipairs(accents) do
        vim.api.nvim_set_hl(0, "RenderMarkdownH" .. i .. "Bg", { fg = c.foreground, bg = bg })
      end
    end

    vim.api.nvim_create_autocmd("ColorScheme", {
      pattern = "pywal16",
      callback = heading_backgrounds,
    })
    heading_backgrounds()

    require("render-markdown").setup({ heading = { icons = {} } })
  end,
}
