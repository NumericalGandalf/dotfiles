if not vim.g.IsDef("sh") then
    require("lspconfig")["bashls"].setup({
        capabilities = require('cmp_nvim_lsp').default_capabilities(),
        filetypes = { "sh", "zsh" }
    })
end
