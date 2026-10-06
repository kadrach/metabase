import userEvent from "@testing-library/user-event";
import { addLocale, useLocale } from "ttag";

import { render, screen } from "__support__/ui";
import { getSemanticTypeName } from "metabase/common/utils/fields";
import { Menu } from "metabase/ui";
import { TYPE } from "metabase-lib/v1/types/constants";

import { SharingButton, SharingMenu } from "./SharingMenu";

const SHARE_BUTTON_CONTEXT =
  "A verb, not a noun. The label of the button that opens the options to share a question or a dashboard.";

describe("SharingMenu", () => {
  describe("with a non-English locale", () => {
    beforeAll(() => {
      addLocale("fr-fake", {
        headers: { "plural-forms": "nplurals=2; plural=(n > 1);" },
        translations: {
          "": { Share: { msgid: "Share", msgstr: ["Part"] } },
          [SHARE_BUTTON_CONTEXT]: {
            Share: { msgid: "Share", msgstr: ["Partager"] },
          },
        },
      });
      useLocale("fr-fake");
    });

    afterAll(() => {
      // restore the default locale so later tests aren't affected
      useLocale("en");
    });

    it("should translate the menu button separately from the Share semantic type (#83050)", async () => {
      render(
        <SharingMenu>
          <Menu.Item>Item</Menu.Item>
        </SharingMenu>,
      );

      await userEvent.hover(screen.getByRole("button", { name: "Partager" }));

      expect(await screen.findByRole("tooltip")).toHaveTextContent("Partager");
      expect(getSemanticTypeName(TYPE.Share)).toBe("Part");
    });

    it("should translate the default label of the button separately from the Share semantic type (#83050)", async () => {
      render(<SharingButton />);

      await userEvent.hover(screen.getByRole("button", { name: "Partager" }));

      expect(await screen.findByRole("tooltip")).toHaveTextContent("Partager");
    });
  });
});
