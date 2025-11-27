-- lazy.nvim plugin manager
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)


require('lazy').setup({
  {
  'uZer/pywal16.nvim',
  lazy = false,
  priority = 1000,
  config = function()
    vim.cmd.colorscheme("pywal16")
  end
  },
  {
    "ThePrimeagen/harpoon",
     branch = "harpoon2",
     dependencies = { "nvim-lua/plenary.nvim" }
  },
  {
    "norcalli/nvim-colorizer.lua",
    config = function()
        require'colorizer'.setup()
    end
  },
  {
    "nvim-tree/nvim-tree.lua",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    config = function()
        require("nvim-tree").setup()
    end
  },
  {
    "nvim-lualine/lualine.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" }
    },
  {
    "nvim-telescope/telescope.nvim",
    branch = '0.1.x',
    dependencies = { "nvim-lua/plenary.nvim" }
    },
  {
    "nvim-treesitter/nvim-treesitter", 
    build = ":TSUpdate",
  },
  {
    "numToStr/Comment.nvim",
    dependencies = {"JoosepAlviste/nvim-ts-context-commentstring"},
    config = function()
        require("Comment").setup()
    end
  },
  {
    "kylechui/nvim-surround",
    version = "*", -- use for stability
    event = "VeryLazy",
    config = function()
        require("nvim-surround").setup({
        })
    end
  },
})

