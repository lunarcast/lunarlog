interface IconProps {
  children: string;
  onClick?: () => void;
  className?: string;
}

export const Icon = ({ children, onClick, className }: IconProps) => {
  return (
    <span onClick={onClick} class={["material-icons", className].join(" ")}>
      {children}
    </span>
  );
};
