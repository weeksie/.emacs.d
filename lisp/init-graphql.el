(require-package 'graphql-mode)

(add-auto-mode 'graphql-mode
               "graphql/types.js" ; a special case for the melty project
               "\\.graphql")


(provide 'init-graphql)
