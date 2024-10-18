{ 
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
}

M.dependencies = {
  "hrsh7th/cmp-nvim-lsp",
  "hrsh7th/cmp-buffer",
  "hrsh7th/cmp-path",
  "hrsh7th/cmp-cmdline",
  "L3MON4D3/LuaSnip",
  "saadparwaiz1/cmp_luasnip",
  "rafamadriz/friendly-snippets",
}

M.event = { "CmdlineEnter", "InsertEnter" }

local function mapping()
  local cmp = require("cmp")
  return {
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
  }
end

local function cmdline_mapping()
  local cmp = require("cmp")
  return {
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
end

function M.config()
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
    mapping = mapping(),
    sources = cmp.config.sources({
      { name = "nvim_lsp" },
      { name = "luasnip" },
    }, {
      { name = "buffer" },
    }),
  })

  cmp.setup.cmdline({ "/", "?" }, {
    mapping = cmdline_mapping(),
    sources = {
      { name = "buffer" },
    },
  })

  cmp.setup.cmdline(":", {
    mapping = cmdline_mapping(),
    sources = cmp.config.sources({
      { name = "path" },
    }, {
      { name = "cmdline" },
    }),
  })

  require("luasnip.loaders.from_vscode").lazy_load()
end

return M
