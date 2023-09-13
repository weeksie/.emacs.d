(define-skeleton just-salad-react-component
  "Provide a templated React Component"
  ""
  "import React from 'react';\n"
  "import Stack from '@components/Stack';\n"
  "\n"
  "interface " (setq component (skeleton-read "Component name: ")) "Props {\n}\n"
  "\n"
  "const " component ": React.FC<" component "Props> = () => {\n"
  "    return (\n"
  "        <Stack>\n"
  "        </Stack>\n"
  "    );\n"
  "};\n"
  "export default " component ";")

(define-skeleton eggbasket-react-component
  "Provide a templated React Component"
  ""
  "import React from 'react';\n"
  "import { Text } from '@shopify/polaris';\n"
  "\n"
  "import Stack from '@components/Stack';\n"
  "\n"
  "interface " (setq component (skeleton-read "Component name: ")) "Props {\n}\n"
  "\n"
  "const " component  ": React.FC<" component "Props> = () => {\n"
  "  return (\n"
  "    <Stack>\n"
  "    </Stack>\n"
  "  );\n"
  "};\n"
  "export default " component ";")

(provide 'init-skeleton)
