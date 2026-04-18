return {
  "stevearc/conform.nvim",
  config = function()
    require("conform").setup({
      formatters_by_ft = {
        python = { "black" },
        lua = { "stylua" },
        javascript = { "prettier" },
        html = { "prettier" },
        css = { "prettier" },
        json = { "prettier" },
        yaml = { "prettier" },
        bash = { "shfmt" },
        sh = { "shfmt" },
      },
    })

    vim.keymap.set({ "n", "v" }, "<leader>p", function()
      require("conform").format({ async = true })
    end, { desc = "Format buffer" })
  end,
}
