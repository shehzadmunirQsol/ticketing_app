import React, { useState } from 'react';
import {
  ColumnDef,
  SortingState,
  VisibilityState,
  flexRender,
  getCoreRowModel,
  getFilteredRowModel,
  getPaginationRowModel,
  getSortedRowModel,
  useReactTable,
} from '@tanstack/react-table';
import { ChevronDown } from 'lucide-react';
import { ScrollArea, ScrollBar } from '~/components/ui/scroll-area';
import { Button } from '@/ui/button';
import {
  DropdownMenu,
  DropdownMenuCheckboxItem,
  DropdownMenuContent,
  DropdownMenuTrigger,
} from '@/ui/dropdown-menu';
import {
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableHeader,
  TableRow,
} from '@/ui/table';
import { trpc } from '~/utils/trpc';
import { customEmailTruncateHandler } from '~/utils/helper';
import { getCustomerSchema } from '~/schema/customer';
import {
  Tooltip,
  TooltipContent,
  TooltipProvider,
  TooltipTrigger,
} from '~/components/ui/tooltip';
import { Switch } from '~/components/ui/switch';
import { CustomerDialog } from '../modal/customers';
import { useToast } from '~/components/ui/use-toast';
import { LoadingDialog } from '../modal/loadingModal';

export type Category = {
  email: string;
  username: string | null;
  first_name: string;
  last_name: string;
  is_approved: boolean;
  is_verified: boolean;
  id: number;
  dob: Date;
  created_at: Date;
  updated_at: Date;
};

export default function CustomersDataTable() {
  // use toast
  const { toast } = useToast();

  // use states
  const [sorting, setSorting] = useState<SortingState>([]);
  const [filters, setFilters] = useState<getCustomerSchema>({
    first: 0,
    rows: 10,
  });
  const [columnVisibility, setColumnVisibility] = useState<VisibilityState>({});
  const [rowSelection, setRowSelection] = useState({});
  const [selectedItem, setSelectedItem] = React.useState({});
  const [title, setTitle] = React.useState('');
  const [type, setType] = React.useState('');
  const [isModal, setIsModal] = React.useState(false);

  // APi
  const { data, refetch, isLoading } = trpc.customer.getCustomers.useQuery(
    filters,
    {
      refetchOnWindowFocus: false,
    },
  );

  const categoryData = React.useMemo(() => {
    return Array.isArray(data?.data) ? data?.data : [];
  }, [data]);

  // handle modal
  const handleEnbled = (data: any, type: string) => {
    if (!data?.is_approved) {
      setSelectedItem(data);
      setTitle('Customer');
      setType(type);
      setIsModal(true);
    } else {
      toast({
        variant: 'success',
        title: `Customer is Already Approved!`,
      });
    }
  };
  // columns
  const columns: ColumnDef<Category>[] = [
    {
      accessorKey: 'email',
      header: 'Email',
      cell: ({ row }) => {
        return (
          <div className="flex items-center gap-4 text-ellipsis whitespace-nowrap overflow-hidden">
            <TooltipProvider>
              <Tooltip>
                <TooltipTrigger>
                  {customEmailTruncateHandler(row?.original?.email)}
                </TooltipTrigger>
                <TooltipContent>
                  <p className="text-base font-normal">
                    {row?.original?.email}
                  </p>
                </TooltipContent>
              </Tooltip>
            </TooltipProvider>
          </div>
        );
      },
    },
    {
      accessorKey: 'username',
      header: 'User Name',
      cell: ({ row }) => (
        <div className="capitalize text-ellipsis whitespace-nowrap ">
          {row.getValue('username')}
        </div>
      ),
    },
    {
      accessorKey: 'first_name',
      header: 'Name',
      cell: ({ row }) => (
        <div className="capitalize text-ellipsis whitespace-nowrap ">
          {row?.original?.first_name + ' ' + row?.original?.last_name}
        </div>
      ),
    },
    {
      id: 'is_verified',
      header: 'Verified Status',

      cell: ({ row }) => {
        return (
          <div>
            <Switch
              checked={row?.original?.is_verified}
              disabled={true}
              // onCheckedChange={() => handleEnbled(row?.original, 'enabled')}
            />
          </div>
        );
      },
    },

    {
      id: 'is_approved',
      header: 'Approved Status',

      cell: ({ row }) => {
        return (
          <div>
            <Switch
              checked={row?.original?.is_approved}
              onCheckedChange={() => handleEnbled(row?.original, 'enabled')}
            />
          </div>
        );
      },
    },
  ];

  const table = useReactTable({
    data: categoryData as Category[],
    columns,
    onSortingChange: setSorting,
    getCoreRowModel: getCoreRowModel(),
    getPaginationRowModel: getPaginationRowModel(),
    getSortedRowModel: getSortedRowModel(),
    getFilteredRowModel: getFilteredRowModel(),
    onColumnVisibilityChange: setColumnVisibility,
    onRowSelectionChange: setRowSelection,
    state: {
      sorting,
      columnVisibility,
      rowSelection,
    },
  });

  function handlePagination(page: number) {
    if (page < 0) return;
    setFilters((prevFilters) => ({ ...prevFilters, first: page }));
  }

  return (
    <div className="w-full space-y-4">
      <div className="flex items-center justify-end gap-2">
        <DropdownMenu>
          <DropdownMenuTrigger asChild>
            <Button variant="outline">
              Columns <ChevronDown className="ml-2 h-4 w-4" />
            </Button>
          </DropdownMenuTrigger>
          <DropdownMenuContent align="end">
            {table
              .getAllColumns()
              .filter((column) => column.getCanHide())
              .map((column) => {
                return (
                  <DropdownMenuCheckboxItem
                    key={column.id}
                    className="capitalize"
                    checked={column.getIsVisible()}
                    onCheckedChange={(value) =>
                      column.toggleVisibility(!!value)
                    }
                  >
                    {column.id}
                  </DropdownMenuCheckboxItem>
                );
              })}
          </DropdownMenuContent>
        </DropdownMenu>
      </div>
      <div className="rounded-md border border-border">
        <ScrollArea className="w-full">
          <ScrollBar orientation="horizontal"></ScrollBar>
          <Table>
            <TableHeader>
              {table?.getHeaderGroups().map((headerGroup) => (
                <TableRow key={headerGroup.id}>
                  {headerGroup.headers.map((header) => {
                    return (
                      <TableHead key={header.id}>
                        {header.isPlaceholder
                          ? null
                          : flexRender(
                              header.column.columnDef.header,
                              header.getContext(),
                            )}
                      </TableHead>
                    );
                  })}
                </TableRow>
              ))}
            </TableHeader>
            <TableBody>
              {table?.getRowModel()?.rows?.length ? (
                table?.getRowModel()?.rows?.map((row) => (
                  <TableRow
                    key={row.id}
                    data-state={row.getIsSelected() && 'selected'}
                  >
                    {row.getVisibleCells().map((cell) => (
                      <TableCell key={cell.id}>
                        {flexRender(
                          cell.column.columnDef.cell,
                          cell.getContext(),
                        )}
                      </TableCell>
                    ))}
                  </TableRow>
                ))
              ) : (
                <TableRow>
                  <TableCell
                    colSpan={columns.length}
                    className="h-24 text-center"
                  >
                    No results.
                  </TableCell>
                </TableRow>
              )}
            </TableBody>
          </Table>
        </ScrollArea>
      </div>
      <div className="flex items-center justify-end space-x-2 py-4">
        <div className="space-x-2">
          <Button
            variant="outline"
            onClick={() => handlePagination(filters.first - 1)}
            disabled={filters.first === 0}
          >
            Previous
          </Button>
          <Button
            variant="outline"
            onClick={() => handlePagination(filters.first + 1)}
            disabled={(filters.first + 1) * filters.rows > (data?.count || 0)}
          >
            Next
          </Button>
        </div>
      </div>
      <CustomerDialog
        selectedItem={selectedItem}
        setSelectedItem={setSelectedItem}
        title={title}
        setTitle={setTitle}
        isModal={isModal}
        setIsModal={setIsModal}
        refetch={refetch}
        type={type}
        setType={setType}
      />
      <LoadingDialog open={isLoading} text={'Loading data...'} />
    </div>
  );
}
