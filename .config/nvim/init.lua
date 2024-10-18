vim.g.mapleader = " "
vim.opt.mouse = ""

vim.opt.termguicolors = true

vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.scrolloff = 10

vim.opt.autoindent = true
vim.opt.smartindent = true
vim.opt.expandtab = true

vim.opt.wrap = true
vim.opt.linebreak = true

vim.opt.autowriteall = false
vim.opt.swapfile = false
vim.opt.backup = false

vim.opt.ignorecase = true
vim.opt.smartcase = true

vim.opt.incsearch = true
vim.opt.hlsearch = false

vim.opt.splitright = true

vim.fn.sign_define("DiagnosticSignError", {
text = "E",
texthl = "DiagnosticSignError",
})
vim.fn.sign_define("DiagnosticSignWarn", {
text = "W",
texthl = "DiagnosticSignWarn",
})
vim.fn.sign_define("DiagnosticSignInfo", {
text = "I",
texthl = "DiagnosticSignInfo",
})
vim.fn.sign_define("DiagnosticSignHint", {
text = "H",
texthl = "DiagnosticSignHint",
})

local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
  if not vim.loop.fs_stat(lazypath) then
    vim.fn.system({
      "git",
      "clone",
      "--filter=blob:none",
      "https://github.com/folke/lazy.nvim.git",
      "--branch=stable",
      lazypath,
    })
  end
  vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
        { 
                "folke/tokyonight.nvim",
lazy = false,
priority = 100,
config = function()
  vim.api.nvim_command("colorscheme tokyonight-night")
end
}, 
{ 
        "nvim-telescope/telescope.nvim",
dependencies = {
  "nvim-lua/plenary.nvim",
  {
    "nvim-telescope/telescope-fzf-native.nvim",
    build = "cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release",
  }, 
  },
  keys = {
  { "<leader>fh", "<cmd>Telescope help_tags<cr>" },
  { "<leader>ff", "<cmd>Telescope find_files<cr>" },
  { "<leader>fn", "<cmd>Telescope buffers<cr>" },
  { "<leader>fg", "<cmd>Telescope live_grep<cr>" },
  { "<leader>fG", "<cmd>Telescope current_buffer_fuzzy_find<cr>" },
  { "<leader>fs", "<cmd>Telescope grep_string<cr>" },
  { "<leader>fc", "<cmd>Telescope git_status<cr>" },
  },
  config = function()
  local telescope = require("telescope")
  local builtin = require("telescope.builtin")
  local actions = require("telescope.actions")

  telescope.setup({
    defaults = {
      mappings = {
        i = {
          ["<C-f>"] = actions.preview_scrolling_down,
          ["<C-b>"] = actions.preview_scrolling_up, 
          ["<C-u>"] = function()
            vim.api.nvim_command("%delete")
          end,
        },
      },
    },
    pickers = {
      find_files = {
        hidden = true,
        follow = true,
      },
      live_grep = {
        additional_args = function()
          return {
            "--hidden",
            "--follow",
            "--smart-case",
            "-g!.git/",
          }
        end,
      },
      grep_string = {
        only_sort_text = true,
      },
    },
    extensions = {
      fzf = {
        fuzzy = true,
        override_generic_sorter = true,
        override_file_sorter = true,
        case_mode = "smart_case",
      },
    },
  })

  telescope.load_extension("fzf")
end 
}, 
{ 
        "nvim-treesitter/nvim-treesitter",
        build = ":TSUpdate",
        lazy = false,
        config = function() 
                require("nvim-treesitter.configs").setup({
    auto_install = true,
    highlight = {
      enable = false,
      additional_vim_regex_highlighting = false,
    },
  })
  end
}, 
{ 
        "williamboman/mason.nvim",
        build = ":MasonUpdate",
        event = "VeryLazy",
        opts = {
    ui = {
      border = "rounded",
    },
        }
}, { 
        "nvim-lualine/lualine.nvim",
        dependencies = "nvim-tree/nvim-web-devicons",
        lazy = false,
        opts = {
    options = {
      globalstatus = true,
      component_separators = {
        left = "",
        right = "│",
      },
      section_separators = {
        left = "",
        right = "",
      },
    },
    sections = {
      lualine_a = {
        "mode",
      },
      lualine_b = {
        {
          "branch",
          icon = "",
        },
      },
      lualine_c = {
        {
          "filename",
          file_status = true,
          path = 1,
        },
        {
          "diagnostics",
          update_in_insert = true,
          symbols = {
            error = "E",
            warn = "W",
            info = "I",
            hint = "H",
          },
        },
        "diff",
      },
      lualine_x = {
        "filetype",
        "filesize",
        "encoding",
        "fileformat",
        "hostname",
      },
      lualine_y = {
        function() 
  local cur = vim.fn.line(".")
  local total = vim.fn.line("$")
  local pog = "Top"
  if cur == total then
    pog = "Bot"
  elseif cur ~= 1 then
    pog = string.format("%2d%%%%", math.floor(cur / total * 100))
  end
  return cur .. ":" .. vim.fn.virtcol(".") .. " (" .. pog .. ")"
        end,
      },
      lualine_z = {},
    },
    extensions = {
      "nvim-dap-ui",
    },
  }
}, { 
        "hrsh7th/nvim-cmp", 
dependencies = {
  "hrsh7th/cmp-nvim-lsp",
  "hrsh7th/cmp-buffer",
  "hrsh7th/cmp-path",
  "hrsh7th/cmp-cmdline",
  "L3MON4D3/LuaSnip",
  "saadparwaiz1/cmp_luasnip",
  "rafamadriz/friendly-snippets",
},
event = { "CmdlineEnter", "InsertEnter" },
config = function()
  local cmp = require("cmp")

  cmp.setup({
    snippet = {
      expand = function(args)
        require("luasnip").lsp_expand(args.body)
      end,
    },
    window = {
      completion = cmp.config.window.bordered(),
      documentation = cmp.config.window.bordered(),
    },
    mapping = {
    ["<C-b>"] = {
      i = cmp.mapping.scroll_docs(-4),
    },
    ["<C-f>"] = {
      i = cmp.mapping.scroll_docs(4),
    },
    ["<C-l>"] = {
      i = function()
        if cmp.visible() then
          cmp.abort()
        else
          cmp.complete()
        end
      end,
    },
    ["<C-p>"] = {
      i = cmp.mapping.select_prev_item({
        behavior = cmp.SelectBehavior.Select,
      }),
    },
    ["<C-n>"] = {
      i = cmp.mapping.select_next_item({
        behavior = cmp.SelectBehavior.Select,
      }),
    },
    ["<C-o>"] = {
      i = cmp.mapping.confirm({
        select = true,
        behavior = cmp.ConfirmBehavior.Replace,
      }),
    },
  },
    sources = cmp.config.sources({
      { name = "nvim_lsp" },
      { name = "luasnip" },
    }, {
      { name = "buffer" },
    }),
  })

  local cmdline_mapping = {
    ["<C-l>"] = {
      c = function()
        if cmp.visible() then
          cmp.abort()
        else
          cmp.complete()
        end
      end,
    },
    ["<C-p>"] = {
      c = cmp.mapping.select_prev_item({
        behavior = cmp.SelectBehavior.Select,
      }),
    },
    ["<C-n>"] = {
      c = cmp.mapping.select_next_item({
        behavior = cmp.SelectBehavior.Select,
      }),
    },
    ["<C-o>"] = {
      c = cmp.mapping.confirm({
        select = true,
        behavior = cmp.ConfirmBehavior.Replace,
      }),
    },
  }
 
  cmp.setup.cmdline({ "/", "?" }, {
    mapping = cmdline_mapping,
    sources = {
      { name = "buffer" },
    },
  })

  cmp.setup.cmdline(":", {
    mapping = cmdline_mapping,
    sources = cmp.config.sources({
      { name = "path" },
    }, {
      { name = "cmdline" },
    }),
  })

  require("luasnip.loaders.from_vscode").lazy_load()
end
},
}, {
    defaults = {
      lazy = true,
    },
    ui = {
      border = "rounded",
    },
    checker = {
      enabled = false,
    },
    profiling = {
      loader = true,
      require = true,
    },
  })
