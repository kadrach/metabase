import { getUserIsAdmin } from "metabase/current-user";
import { PLUGIN_COLLECTION_COMPONENTS } from "metabase/plugins";
import { useSelector } from "metabase/redux";

interface FormAuthorityLevelFieldProps {
  className?: string;
}

export function FormAuthorityLevelField({
  className,
}: FormAuthorityLevelFieldProps) {
  const isAdmin = useSelector(getUserIsAdmin);
  if (!isAdmin) {
    return null;
  }

  return (
    <PLUGIN_COLLECTION_COMPONENTS.FormCollectionAuthorityLevelPicker
      name="authority_level"
      className={className}
    />
  );
}
