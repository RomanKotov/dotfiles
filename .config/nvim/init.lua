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
      "tpope/vim-surround",
      { 
        "nelstrom/vim-visual-star-search",
        config = function()
          local wk = require("which-key")
          wk.add({
            { "<leader>*", desc = "search", mode = { "n" } },
          })
        end
      },
      {
        "ggandor/leap.nvim",
        config = function()
          local wk = require("which-key")
          wk.add({
            { "<leader>j", group = "+jump", mode = { "n", "v", "o" } },
            { "<leader>jj", "<Plug>(leap)",  desc = "current window", mode = { "n", "v", "o" } },
            { "<leader>ja", "<Plug>(leap-anywhere)",  desc = "anywhere", mode = "n" },
            { "<leader>jo", "<Plug>(leap-from-window)",  desc = "other window", mode = "n" },
            { "<leader>jf", "<Plug>(leap-forward)",  desc = "forward", mode = { "n", "v", "o" } },
            { "<leader>jF", "<Plug>(leap-backward)",  desc = "backward", mode = { "n", "v", "o"} },
            { "<leader>jt", "<Plug>(leap-forward-till)",  desc = "forward-till", mode = { "n", "v", "o"} },
            { "<leader>jT", "<Plug>(leap-backward-till)",  desc = "backward-till", mode = { "n", "v", "o"} },
          })
        end
      },
      {
        "christoomey/vim-tmux-navigator",
        init = function() 
          -- Use tmux-navigator mappings
          vim.g.tmux_navigator_no_mappings = 0
          -- Disable tmux navigation when vim is zoomed
          vim.g.tmux_navigator_disable_when_zoomed = 1
        end
      },
      { 
	"junegunn/fzf.vim",
        dependencies = { "junegunn/fzf" },
        config = function() 
          local wk = require("which-key")
          wk.add({
            { "<leader>f", group = "+fzf" },
            { "<leader>ff", "<cmd>Files<CR>",  desc = "find file", mode = "n" },
            { "<leader>fb", "<cmd>Buffers<CR>",  desc = "find buffers", mode = "n" },
            { "<leader>fg", "<cmd>GFiles<CR>",  desc = "git files", mode = "n" },
            { "<leader>fr", "<cmd>RG<CR>",  desc = "interactive rg", mode = "n" },
          })
        end
      },
      -- Language Support
      {
        "nvim-treesitter/nvim-treesitter",
        build = ":TSUpdate",
        event = { "VeryLazy" },
        init = function(plugin)
          require("lazy.core.loader").add_to_rtp(plugin)
          require("nvim-treesitter.query_predicates")
        end,
        opts_extend = { "ensure_installed" },
        cmd = { "TSUpdateSync", "TSUpdate", "TSInstall" },
        opts = {
          highlight = { enable = true },
          indent = { enable = true },
          ensure_installed = {
            "bash",
            "c",
            "diff",
            "eex",
            "elixir",
            "html",
            "heex",
            "javascript",
            "jsdoc",
            "json",
            "jsonc",
            "lua",
            "luadoc",
            "luap",
            "markdown",
            "markdown_inline",
            "printf",
            "python",
            "query",
            "regex",
            "toml",
            "tsx",
            "typescript",
            "vim",
            "vimdoc",
            "xml",
            "yaml",
          },
        },
        config = function(_, opts)
          require("nvim-treesitter.configs").setup(opts)
        end,
      },
      -- LSP Support
      {'neovim/nvim-lspconfig'},
      {'williamboman/mason.nvim'},
      {'williamboman/mason-lspconfig.nvim'},

      -- Autocompletion
      {'hrsh7th/cmp-nvim-lsp'},
      {'hrsh7th/cmp-buffer'},
      {'hrsh7th/cmp-path'},
      {'hrsh7th/nvim-cmp'},

      -- GIT Support
      {'lewis6991/gitsigns.nvim'},
      {'tpope/vim-fugitive'},
    },
    -- colorscheme that will be used when installing plugins.
    install = { colorscheme = { "evening" } },
    -- automatically check for plugin updates
    checker = { enabled = true },
  })
 
-- Styling section
vim.cmd("set relativenumber")
vim.cmd("set numberwidth=4")
vim.cmd("set cursorline")
vim.cmd("language en_US.UTF-8")
vim.cmd("colorscheme evening")

-- Editing section
vim.cmd("set incsearch")
vim.cmd("set hlsearch")
vim.cmd("set ignorecase")
vim.cmd("set infercase")

-- Netrw section
vim.cmd("let g:netrw_liststyle=3") -- tree-node by derault
vim.cmd("let g:netrw_preview=1") -- vertical preview
-- vim.cmd("let g:netrw_browse_split=4") -- open file in previous buffer

---
-- LSP setup
---

-- Reserve a space in the gutter
-- This will avoid an annoying layout shift in the screen
vim.opt.signcolumn = 'yes'

-- Add cmp_nvim_lsp capabilities settings to lspconfig
-- This should be executed before you configure any language server
local lspconfig_defaults = require('lspconfig').util.default_config
lspconfig_defaults.capabilities = vim.tbl_deep_extend(
  'force',
  lspconfig_defaults.capabilities,
  require('cmp_nvim_lsp').default_capabilities()
)

-- This is where you enable features that only work
-- if there is a language server active in the file
vim.api.nvim_create_autocmd('LspAttach', {
  desc = 'LSP actions',
  callback = function()
    local wk = require('which-key')
    wk.add({
      {'K', '<cmd>lua vim.lsp.buf.hover()<cr>', desc = 'show docs', mode = 'n' },
      {'gd', '<cmd>lua vim.lsp.buf.definition()<cr>', desc = 'go to definition', mode = 'n' },
      {'gD', '<cmd>lua vim.lsp.buf.declaration()<cr>', desc = 'go to declaration', mode = 'n' },
      {'gi', '<cmd>lua vim.lsp.buf.implementation()<cr>', desc = 'go to implementation', mode = 'n' },
      {'go', '<cmd>lua vim.lsp.buf.type_definition()<cr>', desc = 'go to type definition', mode = 'n' },
      {'gr', '<cmd>lua vim.lsp.buf.references()<cr>', desc = 'show references', mode = 'n' },
      {'gs', '<cmd>lua vim.lsp.buf.signature_help()<cr>', desc = 'signature help', mode = 'n' },
      {'<F2>', '<cmd>lua vim.lsp.buf.rename()<cr>', desc = 'rename symbol', mode = 'n' },
      {'<F3>', '<cmd>lua vim.lsp.buf.format({async = true})<cr>', desc = 'format file', mode = {'n', 'x'} },
      {'<F4>', '<cmd>lua vim.lsp.buf.code_action()<cr>', desc = 'code action', mode = 'n' },
    })
  end,
})

require('mason').setup({})
require('mason-lspconfig').setup({
  handlers = {
    function(server_name)
      require('lspconfig')[server_name].setup({})
    end,
  }
})

---
-- Autocompletion config
---
local cmp = require('cmp')

cmp.setup({
  sources = {
    {name = 'nvim_lsp'},
  },
  mapping = cmp.mapping.preset.insert({
    -- `Enter` key to confirm completion
    ['<CR>'] = cmp.mapping.confirm({select = false}),

    -- Ctrl+Space to trigger completion menu
    ['<C-Space>'] = cmp.mapping.complete(),

    -- Scroll up and down in the completion documentation
    ['<C-u>'] = cmp.mapping.scroll_docs(-4),
    ['<C-d>'] = cmp.mapping.scroll_docs(4),
  }),
  snippet = {
    expand = function(args)
      vim.snippet.expand(args.body)
    end,
  },
})

vim.diagnostic.config({
    virtual_text = true,
    signs = true,
    update_in_insert = false,
    underline = true,
    severity_sort = false,
    float = true,
  })



---
-- GIT config
---
require('gitsigns').setup {
  on_attach = function(bufnr) 
    local gitsigns = require('gitsigns')
    local wk = require('which-key')

    wk.add({ "<leader>h", group = "+hunks", mode = { "n", "v" } })
    wk.add({ "<leader>ht", group = "+toggle" })

    local function map(mode, l, desc, r)
      wk.add({ l, r, mode = mode, desc = desc })
    end

-- Navigation
    map('n', ']c', 'next hunk', function()
      if vim.wo.diff then
        vim.cmd.normal({']c', bang = true})
      else
        gitsigns.nav_hunk('next')
      end
    end)

    map('n', '[c', 'previous hunk', function()
      if vim.wo.diff then
        vim.cmd.normal({'[c', bang = true})
      else
        gitsigns.nav_hunk('prev')
      end
    end)

    -- Actions
    map('n', '<leader>hs', 'stage hunk', gitsigns.stage_hunk)
    map('n', '<leader>hr', 'reset hunk', gitsigns.reset_hunk)

    map('v', '<leader>hs', 'stage hunk', function()
      gitsigns.stage_hunk({ vim.fn.line('.'), vim.fn.line('v') })
    end)

    map('v', '<leader>hr', 'reset hunk', function()
      gitsigns.reset_hunk({ vim.fn.line('.'), vim.fn.line('v') })
    end)

    map('n', '<leader>hS', 'stage buffer', gitsigns.stage_buffer)
    map('n', '<leader>hR', 'reset buffer', gitsigns.reset_buffer)
    map('n', '<leader>hp', 'preview', gitsigns.preview_hunk)
    map('n', '<leader>hi', 'preview inline', gitsigns.preview_hunk_inline)

    map('n', '<leader>hb', 'blame', function()
      gitsigns.blame_line({ full = true })
    end)

    map('n', '<leader>hd', 'diff', gitsigns.diffthis)

    map('n', '<leader>hD', 'diff parent', function()
      gitsigns.diffthis('~')
    end)

    map('n', '<leader>hQ', 'populate locations all', function() gitsigns.setqflist('all') end)
    map('n', '<leader>hq', 'populate locations', gitsigns.setqflist)

    -- Toggles
    map('n', '<leader>htb', 'current line blame', gitsigns.toggle_current_line_blame)
    map('n', '<leader>htw', 'word diff', gitsigns.toggle_word_diff)

    -- Text object
    map({'o', 'x'}, 'ih', 'hunk', gitsigns.select_hunk)
  end
}
