local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
  local lazyrepo = "https://github.com/folke/lazy.nvim.git"
  local out = vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
  if vim.v.shell_error ~= 0 then
    vim.api.nvim_echo({
      { "Failed to clone lazy.nvim:\n", "ErrorMsg" },
      { out, "WarningMsg" },
      { "\nPress any key to exit..." },
    }, true, {})
    vim.fn.getchar()
    os.exit(1)
  end
end
vim.opt.rtp:prepend(lazypath)

-- Make sure to setup `mapleader` and `maplocalleader` before
-- loading lazy.nvim so that mappings are correct.
-- This is also a good place to setup other settings (vim.opt)
vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

-- Setup lazy.nvim
require("lazy").setup({
  spec = {
    "folke/which-key.nvim",
    "nelstrom/vim-visual-star-search",
    "tpope/vim-surround",
    {
      "christoomey/vim-tmux-navigator",
      init = function() 
        -- Use tmux-navigator mappings
        vim.g.tmux_navigator_no_mappings = 0
        -- Disable tmux navigation when vim is zoomed
        vim.g.tmux_navigator_disable_when_zoomed = 1
      end
    },
    -- Language Support
    {
      'VonHeikemen/lsp-zero.nvim',
      branch = 'v1.x',
      dependencies = {
        -- LSP Support
        {'neovim/nvim-lspconfig'},
        {'williamboman/mason.nvim'},
        {'williamboman/mason-lspconfig.nvim'},

	  -- Autocompletion
	  {'hrsh7th/nvim-cmp'},
	  {'hrsh7th/cmp-nvim-lsp'},
	  {'hrsh7th/cmp-buffer'},
	  {'hrsh7th/cmp-path'},
	},
	config = function() 
	  local lsp = require('lsp-zero')
	  lsp.preset('recommended')
	  lsp.setup()
	  vim.diagnostic.config({
	    virtual_text = true,
	    signs = true,
	    update_in_insert = false,
	    underline = true,
	    severity_sort = false,
	    float = true,
	  })
	end
        },
      },
      -- colorscheme that will be used when installing plugins.
      install = { colorscheme = { "habamax" } },
      -- automatically check for plugin updates
      checker = { enabled = true },
    })

-- Styling section
vim.cmd("set relativenumber")
vim.cmd("set numberwidth=4")
vim.cmd("set cursorline")
vim.cmd("language en_US")

-- Editing section
vim.cmd("set incsearch")
vim.cmd("set hlsearch")
vim.cmd("set ignorecase")
vim.cmd("set infercase")
