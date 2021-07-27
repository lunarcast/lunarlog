interface IconProps {
  children: string;
}

export const Icon = ({ children }: IconProps) => {
  return <span class="material-icons">{children}</span>;
};
